import pandas as pd

# SURVSTAT

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/Survstat/'

file_dict = {
    'pneumococcal_disease_reporting_triangle_survstat.csv' : 'reporting_triangle-survstat-pneumococcal.csv',
    'pneumococcal_disease_reporting_triangle_survstat_preprocessed.csv' : 'reporting_triangle-survstat-pneumococcal-preprocessed.csv',
    'rsv_infection_reporting_triangle_survstat.csv': 'reporting_triangle-survstat-rsv.csv',
    'rsv_infection_reporting_triangle_survstat_preprocessed.csv': 'reporting_triangle-survstat-rsv-preprocessed.csv',
    'seasonal_influenza_reporting_triangle_survstat.csv': 'reporting_triangle-survstat-influenza.csv',
    'seasonal_influenza_reporting_triangle_survstat_preprocessed.csv': 'reporting_triangle-survstat-influenza-preprocessed.csv'
}

for i in file_dict.keys():
    print(i)
    if 'pneumococcal' in i:
        target_path = '../data/survstat/pneumococcal/'
    elif 'rsv' in i:
        target_path = '../data/survstat/rsv/'
    elif 'influenza' in i:
        target_path = '../data/survstat/influenza/'
    
    df = pd.read_csv(path + i)
    df.to_csv(target_path + file_dict[i], index = False)


# NRZ

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/NRZ/'

file_dict = {
    'influenza_reporting_triangle.csv' : 'reporting_triangle-nrz-influenza.csv',
    'influenza_reporting_triangle_preprocessed.csv' : 'reporting_triangle-nrz-influenza-preprocessed.csv',
    'rsv_reporting_triangle.csv' : 'reporting_triangle-nrz-rsv.csv',
    'rsv_reporting_triangle_preprocessed.csv' : 'reporting_triangle-nrz-rsv-preprocessed.csv'
}

for i in file_dict.keys():
    print(i)
    if 'rsv' in i:
        target_path = '../data/nrz/rsv/'
    elif 'influenza' in i:
        target_path = '../data/nrz/influenza/'
    
    df = pd.read_csv(path + i)
    df.to_csv(target_path + file_dict[i], index = False)


# ICOSARI

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/SARI/'

file_dict = {
    'SARI_reporting_triangle.csv' : 'reporting_triangle-icosari-sari.csv',
    'SARI_reporting_triangle_preprocessed.csv' : 'reporting_triangle-icosari-sari-preprocessed.csv'
}

for i in file_dict.keys():
    print(i)
    target_path = '../data/icosari/sari/'
    
    df = pd.read_csv(path + i)
    df.to_csv(target_path + file_dict[i], index = False)
