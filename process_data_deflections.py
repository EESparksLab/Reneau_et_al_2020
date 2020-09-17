#!/usr/bin/env python
'''
Script for processing corn pusher data
*Data should be placed in a folder called "data" in the same directory as this script

Removes pull away and static loading data
Finds sets of monotonically increasing data
Determines "good" push trials by keeping sets with sufficent length

Outputs .csv file with only the best (most consecutive pts) trial for each plant/root

Author: Adam Stager
Contact: astager@udel.edu
'''

import os
import csv
import numpy as np
import matplotlib.pyplot as plt
import math
import sys
import time
from scipy import stats


class PusherData:
    def __init__(self, data_path, deflection_tol, load_tol, min_points, deflection_range, \
                 operator_row=2, plant_row=9, note_row=21, data_row=38, \
                 angle_col = 2, load_col=3):
        self.data_path = data_path
        self.deflection_tol = deflection_tol
        self.load_tol = load_tol
        self.min_points = min_points
        self.deflection_range = deflection_range
        self.operator_row = operator_row  # operator row from raw pusher .csv
        self.plant_row = plant_row        # plant row from raw pusher .csv
        self.note_row = note_row          # test note row from raw pusher .csv
        self.data_row = data_row          # data begins on this row from raw pusher .csv
        self.angle_col = angle_col        # raw_data column - 1 for potentiometer, 2 for imu
        self.load_col = load_col          # raw_data column containing load data


    def extract_files(self):
        files = []  # extract file paths from all folders in the data directory
        path = self.data_path
        for r, d, f in os.walk(path):  # r=root, d=directories, f = files
            for file in f:
                if '.csv' in file:
                    files.append(os.path.join(r, file))

        files.sort()
        print(files)
        return files


    def process_files(self, files, draw_plots):
        self.save_trial_data('filename','operator', 'plot', 'plant', 'whirl_id', 'line_raw_slope (N/m)', 'line_push_slope (N/m)', 'ransac_slope(N/m)', 'ratio captured', 'points')
        for filename in files:
            raw_data, metadata = self.load_data(filename)
            self.display_file_info(filename, metadata)
            filtered_data, data, longest_trial, max_pts = self.filter_data(raw_data)
            m, c, ratio = self.get_ransac_slopes(filtered_data, longest_trial)
            m0, c0 = self.get_line(data)  # fit line to all data
            m1, c1 = self.get_line(filtered_data)  # fit line to all data
            self.save_trial_data(filename, metadata[0], metadata[1], metadata[2], metadata[3], m0, m1, m, ratio, max_pts)
            if draw_plots:
                self.plot_trials(filtered_data, data)
                self.plot_line_fit(m,c,'blue','ransac') # blue is ransac fit to longest consecutive trial
                self.plot_line_fit(m0,c0,'green','raw fit') # orange is line fit to raw loop data
                self.plot_line_fit(m1,c1,'magenta','pushing fit')  # green is line fit to just pushing trials
                plt.show()

    def get_line(self, data):
        data_len = len(data)
        x = np.zeros(data_len)
        y = np.zeros(data_len)
        for i in range(data_len):
            x[i] = data[i][0]
            y[i] = data[i][1]
        try:
            slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)
            return slope, intercept
        except:
            print('no data in this row - setting slope and intercept as zero')
            return 0, 0


    def load_data(self, filename):
        """ 
        Load raw data from pusher .csv file

        :param filename (string) - PATH to data or just filenamee.csv if in same directory
        :return raw_data (list) - len(raw_data) rows
                                  rows format [time, angle_pot, angle_imu, load_x, load_y]
        :return metadata (list) - [operator, plot, plant, whirl]
        """
        # Initialize and fill raw_data array
        raw_data = []
        with open(filename) as csv_file:
            csv_reader = csv.reader(csv_file, delimiter=',')
            line_count = 0
            for row in csv_reader:
                if line_count >= self.data_row:
                    raw_data.append(row)
                elif line_count == self.operator_row:
                    operator = row[1]
                elif line_count == self.plant_row:
                    plant = row[1]
                elif line_count == self.note_row:
                    whirl = row[1]
                line_count += 1
            #print('Lines of raw data processed: ' + str(line_count))

            split_filename = filename.split('/')
            plot = split_filename[len(split_filename)-2]  # plot is folder name

        return raw_data, [operator, plot, plant, whirl]


    def display_file_info(self, filename, metadata):
        print('\n')
        print('Processing: ' + str(filename))
        print('##### ' + 'Operator: ' + str(metadata[0])+ ' |' + ' Plot: ' + str(metadata[1])+ ' |' \
                       + ' Plant: ' + str(metadata[2])+ ' |'+ ' Whirl: ' + str(metadata[3]))


    def filter_data(self, raw_data):
        """ 
        reformat data into [deflection, load, push_direction]
        remove data if load does not change more than load tolerance in a timestep
        remove wind noise by including only trials with enough consecutively increasing data points
        :param raw_data - row [time, angle_pot, angle_imu, load_x, load_y]
        :param load_tolerance - amount load needed between data points
        :param min_trial_len - min number of monotonically increasing points in a pushing dataset
        :param plot_pullaways - True/False
        :return filtered_data - [deflection, load, trial#]
        """
        data_len = len(raw_data) # number of datapoints collected
        data = np.ones((data_len, 3))  # init array to store [deflection, load, direction]
        filtered_data = np.zeros((data_len, 3))  # [deflection, load, trial]
        start_index = 0   # start of new push trial
        end_index = 0     # end of new push trial
        new_trial = 0     # indicator for new trial
        curr_trial = 0         # push trial counter
        most_consec_data = 0
        longest_trial = 0

        '''def reset_counters():
            start_index = 0   # start of new push trial
            end_index = 0     # end of new push trial
            new_trial = 0     # reset new trial indicator
            print('reseting counters')'''

        for i in range(data_len):
            # Reformat data - [deflection (m), load, direction]
            # direction: data[i][2]=1 is push, data[i][2]=-1 pull away
            temp_raw_angle = float(raw_data[i][self.angle_col])
            data[i][0] = 0.64*math.sin(3.1415*temp_raw_angle/180.0)
            data[i][1] = float(raw_data[i][self.load_col])
            
            # Filter data
            if i > 0 and i < data_len-1:

                # Push deflection is not increasing, assume pull away is occuring [-1 state]
                if data[i+1][0]-data[i][0] < 0.0 and data[i][0]-data[i-1][0] < 0.0:  
                    data[i][2] = -1
                    start_index = 0   # start of new push trial
                    end_index = 0     # end of new push trial
                    new_trial = 0     # reset new trial indicator

                # Push deflection not far enough in a single timestep
                elif data[i][0]-data[i-1][0] < self.deflection_tol:  
                    start_index = 0   # start of new push trial
                    end_index = 0     # end of new push trial
                    new_trial = 0     # reset new trial indicator

                # Load is not changing enough, user is not pushing forward
                elif data[i][1]-data[i-1][1] < self.load_tol:  
                    start_index = 0   # start of new push trial
                    end_index = 0     # end of new push trial
                    new_trial = 0     # reset new trial indicator

                # If none of above, then its good data - save it
                else:
                    if start_index == 0:
                        start_index = i
                        end_index = i
                        ##print('######  POTENTIAL NEW TRIAL  #######')
                        ##print('start index:' + str(start_index))
                    else:
                        end_index = i
                    consec_data = end_index - start_index
                        
                    ##print('consecutive data: ' + str(consec_data) + ' gap: ' + str(data[end_index][0]-data[start_index][0]))

                    # Check for enough consecutive good data points
                    if consec_data >= self.min_points:
                        # Check deflection range is large enough
                        if data[end_index][0]-data[start_index][0] >= self.deflection_range:
                            if new_trial == 0:
                                new_trial = 1
                                curr_trial += 1
                                ##print('current trial: ' + str(curr_trial))
                                for j in range(consec_data):
                                    filtered_data[i-j][0] = data[i-j][0]
                                    filtered_data[i-j][1] = data[i-j][1]
                                    filtered_data[i-j][2] = curr_trial
                            else:
                                # Save good data to current trial
                                filtered_data[i][0] = data[i][0]
                                filtered_data[i][1] = data[i][1]
                                filtered_data[i][2] = curr_trial

                            if consec_data > most_consec_data:
                                longest_trial = curr_trial
                                most_consec_data = consec_data

        # Remove bad data
        bad_data_tracker = []
        for i in range(len(filtered_data)):
            if filtered_data[i][2] == 0:
                bad_data_tracker.append(i)
        filtered_data = np.delete(filtered_data, bad_data_tracker, 0)

        #print(filtered_data)
        #print(longest_trial)
        return filtered_data, data, longest_trial, most_consec_data


    def get_ransac_slopes(self, filtered_data, longest_trial):
        # Check if there are good trials and save bad ones with zeros
        if longest_trial > 0:
            print('##### Trial ' + str(longest_trial))
            temp_x = []  # this list will hold x points 
            temp_y = []
            for j in range(len(filtered_data)):
                if filtered_data[j, 2] == longest_trial:  # need to add one because we named first trial 1, not 0
                    temp_x = temp_x + [[filtered_data[j,0]]]
                    temp_y = temp_y + [[filtered_data[j,1]]]
            temp_points = np.hstack( (temp_x,temp_y) )
            #print('data points: ', temp_points)

            ratio, m, c = self.run_ransac(temp_points, temp_x)

            return m, c, ratio
        else:
            return 0,0,0


    def run_ransac(self, data, x_points):
        # Ransac parameters
        ransac_iterations = 500  # number of iterations tested
        ransac_threshold = 0.005 # allowable error threshold (0.5 for imu data, or 0.1 for pot data)
        ransac_ratio = 0.7      # ratio of inliers required to assert
                            # that a model fits well to data
        ratio = 0.
        model_m = 0.
        model_c = 0.
     
        n_samples = len(data)
    
        # perform RANSAC iterations
        for it in range(ransac_iterations):
     
            # pick up two random points
            n = 2
     
            all_indices = np.arange(len(data))
            np.random.shuffle(all_indices)
     
            indices_1 = all_indices[:n]
            indices_2 = all_indices[n:]
     
            maybe_points = data[indices_1,:]
            test_points = data[indices_2,:]
     
            # find a line model for these points
            m, c = self.find_line_model(maybe_points)
     
            x_list = []
            y_list = []
            num = 0
     
            # find orthogonal lines to the model for all testing points
            for ind in range(test_points.shape[0]):
     
                x0 = test_points[ind,0]
                y0 = test_points[ind,1]
     
                # find an intercept point of the model with a normal from point (x0,y0)
                x1, y1 = self.find_intercept_point(m, c, x0, y0)
     
                # distance from point to the model
                dist = math.sqrt((x1 - x0)**2 + (y1 - y0)**2)
     
                # check whether it's an inlier or not
                if dist < ransac_threshold:
                    x_list.append(x0)
                    y_list.append(y0)
                    num += 1
     
            x_inliers = np.array(x_list)
            y_inliers = np.array(y_list)
     
            # in case a new model is better - cache it
            if num/float(n_samples) > ratio:
                ratio = num/float(n_samples)
                model_m = m
                model_c = c 

        print('#####    Slope: ' + str(model_m) + ', Fit ratio: ' + str(ratio))
    
        return ratio, model_m, model_c

 
    def find_line_model(self, points):
        """ find a line model for the given points
        :param points selected points for model fitting
        :return line model
        """
     
        # [WARNING] vertical and horizontal lines should be treated differently
        #           here we just add some noise to avoid division by zero
     
        # find a line model for these points
        m = (points[1,1] - points[0,1]) / (points[1,0] - points[0,0] + sys.float_info.epsilon)  # slope (gradient) of the line
        c = points[1,1] - m * points[1,0]                                     # y-intercept of the line
     
        return m, c

    def find_intercept_point(self, m, c, x0, y0):
        """ find an intercept point of the line model with
            a normal from point (x0,y0) to it
        :param m slope of the line model
        :param c y-intercept of the line model
        :param x0 point's x coordinate
        :param y0 point's y coordinate
        :return intercept point
        """
     
        # intersection point with the model
        x = (x0 + m*y0 - m*c)/(1 + m**2)
        y = (m*x0 + (m**2)*y0 - (m**2)*c)/(1 + m**2) + c
     
        return x, y


    def save_trial_data(self, filename, opr, plt, plnt, wrl, slp0, slp1, slp, fit, pts):
        data_buffer = str(filename) + ',' +str(opr) + ',' + str(plt) + ',' + str(plnt) + ',' + str(wrl)+',' + str(slp0)+',' + str(slp1)+',' + str(slp) + ',' + str(fit) + ',' + str(pts) +'\n'
        data_csv.write(data_buffer)


    def plot_line_fit(self, slope, intercept, c, label_name):
        """Plot a line from slope and intercept"""
        axes = plt.gca()
        x_vals = np.array(axes.get_xlim())
        y_vals = intercept + slope * x_vals
        plt.plot(x_vals, y_vals, color=c, linestyle='--', label=label_name)
        plt.legend()

    def plot_trials(self, filtered_data, data):
        num_filtered_data = len(filtered_data)
        num_data = len(data)
        plot_pullaways = True

        # Plot all data (o pushing, x pulling away)
        if plot_pullaways:
            for i in range(num_data):
                if data[i][2] == 1:
                    plt.plot(data[i][0], data[i][1], 'ok', alpha = 0.1)
                else:
                    plt.plot(data[i][0], data[i][1], 'rx', alpha = 0.1)
    
        for i in range(num_filtered_data):
            if filtered_data[i][2] == 1:
                plt.plot(filtered_data[i][0], filtered_data[i][1], 'oy', alpha = 0.5)
            elif filtered_data[i][2] == 2:
                plt.plot(filtered_data[i][0], filtered_data[i][1], 'or', alpha = 0.5)
            elif filtered_data[i][2] == 3:
                plt.plot(filtered_data[i][0], filtered_data[i][1], 'oc', alpha = 0.5)
            elif filtered_data[i][2] == 4:
                plt.plot(filtered_data[i][0], filtered_data[i][1], 'ob', alpha = 0.5)
            else:
                print('A fifth push? Probably more than one line in a trial...')
                plt.plot(filtered_data[i][0], filtered_data[i][1], 'og', alpha = 0.5)



def start_data_collection():
    ### Open file for writing ###
    global data_csv
    currentDate = time.strftime('%m_%d_%y__%H_%M_%S')
    data_csv = open("./data_%s.csv" %currentDate,"w")


############# BEGIN MAIN ###############
if __name__ == "__main__":

    deflection_tolerance = 0.001 # values along x-axis, must push at least 0.001m per timestep
    load_tolerance = 0.01    # values along y-axis
    minimum_datapoints = 10  # set minimum consecutively increasing datapoints required per push
    deflection_range = 0.02 # must push at least 0.02m to be a trial
    draw_plots = True       # check every plot by setting True, run everything fast with False

    start_data_collection()

    pusher_data = PusherData('./data', \
                             deflection_tolerance, load_tolerance, minimum_datapoints, deflection_range)
    curr_files = pusher_data.extract_files()
    pusher_data.process_files(curr_files, draw_plots)


