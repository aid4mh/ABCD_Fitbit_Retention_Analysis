import pandas as pd
import glob
import tarfile
from io import BytesIO
import time

def combineDatasets(file_range_min, file_range_max, data_type, source_folder_path, follow_up):
    """"""
    if follow_up == True:
        file_extension = '2_year_follow_up_y_arm_1.tar.gz'
    elif follow_up == False:
        file_extension = 'baseline_year_1_arm_1.tar.gz'

    file_list = glob.glob(f'{source_folder_path}/*{file_extension}')

    list_of_datasets = []
    #participant_id_list = []
    for file in file_list:
        file_dir = file
        # Open the tar.gz file
        tar = tarfile.open(file_dir, "r:gz")
        # Select the file that matches the selected data type
        for member in tar.getmembers():
            if data_type in member.name:
                dataset = tar.extractfile(member)
                # Split on data type, take first split and remove the underscore
                participant_id = member.name.split(data_type)[0][:-1]
                #participant_id_list.append(participant_id)
                if dataset is not None:
                    content = dataset.read()
                    # Returns bytes, so convert with BytesIO
                    df = pd.read_csv(BytesIO(content))
                    df['participant_id'] = participant_id
                    if follow_up == True:
                        df['eventname'] = '2_year_follow_up_y_arm_1'
                    elif follow_up == False:
                        df['eventname'] = 'baseline_year_1_arm_1'
                    list_of_datasets.append(df)
    return list_of_datasets

def mergeDatasets(input_data_type):
    input_path = '/Users/ethankim/aid4mh/abcdstudy_fitbit_da/raw_fitbit/aurora01'
    output_path = f'{input_path}/{input_data_type}_merged.csv'
    file_range_min = 1
    file_range_max = 7118
    print(f'Merging {input_data_type} baseline data...')
    baseline_data = combineDatasets(
            file_range_min=file_range_min, file_range_max=file_range_max,
            data_type = input_data_type, source_folder_path = input_path, follow_up = False
    )
    baseline_data = pd.concat(baseline_data)
    print(f'Merging {input_data_type} 2 year follow up data...')
    year_2_data = combineDatasets(
            file_range_min=file_range_min, file_range_max=file_range_max,
            data_type = input_data_type, source_folder_path = input_path, follow_up = True
    )
    year_2_data = pd.concat(year_2_data)
    print('Standardizing column names...')
    if input_data_type == "heartrate_1min":
        baseline_data = baseline_data.rename(columns = {'Time':'Wear_Time'})
    elif input_data_type == 'minuteSleep':
        year_2_data = year_2_data.rename(columns = {'value':'Value'})
        baseline_data = baseline_data.rename(columns = {'date':'Wear_Time', 'value':'Value'}).drop(['logId'], axis = 1)
    elif input_data_type == 'minuteStepsNarrow':
        year_2_data = year_2_data.rename(columns = {'Steps':'Value'})
        baseline_data = baseline_data.rename(columns = {
            'ActivityMinute':'Wear_Time',
            'Steps':'Value'
        })
    print(f'Appending baseline and 2-year follow up DataFrames for {input_data_type}...')
    merged_df = baseline_data.append(year_2_data)
    print(f'Typing columns...')
    merged_df['Value'] = merged_df['Value'].astype('int16')
    print(f'Writing to csv: {output_path}...')
    merged_df.to_csv(output_path, index = False)

data_types = ['minuteStepsNarrow']
for type in data_types:
    print(f'Processing {type}...')
    start = time.time()
    mergeDatasets(input_data_type = type)
    end = time.time()
    duration = end-start
    print(f'Finished processing {type} in {duration} seconds.')
