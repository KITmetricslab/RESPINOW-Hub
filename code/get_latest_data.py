import pandas as pd

# SURVSTAT

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/Survstat/'

file_dict = {
    'latest_data-Survstat-influenza.csv' : 'latest_data-survstat-influenza.csv',
    'latest_data-Survstat-rsv.csv': 'latest_data-survstat-rsv.csv',
    'latest_data-Survstat-pneumococcal.csv': 'latest_data-survstat-pneumococcal.csv'
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
    'latest_data-NRZ-influenza.csv' : 'latest_data-nrz-influenza.csv',
    'latest_data-NRZ-rsv.csv': 'latest_data-nrz-rsv.csv',
    'latest_data-NRZ-influenza-tests.csv' : 'latest_data-nrz-influenza-tests.csv',
    'latest_data-NRZ-rsv-tests.csv' : 'latest_data-nrz-rsv-tests.csv'
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
    'latest_data-SARI-sari.csv' : 'latest_data-icosari-sari.csv',
    'latest_data-SARI-sari_covid19.csv' : 'latest_data-icosari-sari_covid19.csv',
    'latest_data-SARI-sari_influenza.csv' : 'latest_data-icosari-sari_influenza.csv',
    'latest_data-SARI-sari_rsv.csv' : 'latest_data-icosari-sari_rsv.csv'
}

for i in file_dict.keys():
    print(i)
    if 'sari_covid19' in i:
        target_path = '../data/icosari/sari_covid19/'
    elif 'sari_influenza' in i:
        target_path = '../data/icosari/sari_influenza/'
    elif 'sari_rsv' in i:
        target_path = '../data/icosari/sari_rsv/'
    else:
        target_path = '../data/icosari/sari/'
        
    df = pd.read_csv(path + i)
    df.to_csv(target_path + file_dict[i], index = False)


# CVN

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/CVN/'

file_dict = {
    'latest_data-CVN-influenza.csv' : 'latest_data-cvn-influenza.csv',
    'latest_data-CVN-rsv.csv': 'latest_data-cvn-rsv.csv',
    'latest_data-CVN-pneumococcal.csv': 'latest_data-cvn-pneumococcal.csv',
    'latest_data-CVN-influenza-tests.csv' : 'latest_data-cvn-influenza-tests.csv',
    'latest_data-CVN-rsv-tests.csv': 'latest_data-cvn-rsv-tests.csv',
    'latest_data-CVN-pneumococcal-tests.csv': 'latest_data-cvn-pneumococcal-tests.csv'
}

for i in file_dict.keys():
    print(i)
    if 'pneumococcal' in i:
        target_path = '../data/cvn/pneumococcal/'
    elif 'rsv' in i:
        target_path = '../data/cvn/rsv/'
    elif 'influenza' in i:
        target_path = '../data/cvn/influenza/'
    
    df = pd.read_csv(path + i)
    df.to_csv(target_path + file_dict[i], index = False)


# AGI

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/AGI/'

file_dict = {
    'latest_data-AGI-are.csv' : 'latest_data-agi-are.csv',
}

for i in file_dict.keys():
    print(i)
    target_path = '../data/agi/are/'
    
    df = pd.read_csv(path + i)
    df.to_csv(target_path + file_dict[i], index = False)
