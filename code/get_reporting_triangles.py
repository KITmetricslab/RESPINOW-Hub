import pandas as pd

# Survstat

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/Survstat/'

files = [
    'reporting_triangle-survstat-pneumococcal.csv',
    'reporting_triangle-survstat-pneumococcal-preprocessed.csv',

    'reporting_triangle-survstat-rsv.csv',
    'reporting_triangle-survstat-rsv-preprocessed.csv',

    'reporting_triangle-survstat-influenza.csv',
    'reporting_triangle-survstat-influenza-preprocessed.csv'
]

for f in files:
    print(f)
    if 'pneumococcal' in f:
        target_path = '../data/survstat/pneumococcal/'
    elif 'rsv' in f:
        target_path = '../data/survstat/rsv/'
    elif 'influenza' in f:
        target_path = '../data/survstat/influenza/'
    
    df = pd.read_csv(path + f)
    df.to_csv(target_path + f, index = False)


# NRZ

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/NRZ/'

files = [
    'reporting_triangle-nrz-influenza.csv',
    'reporting_triangle-nrz-influenza-preprocessed.csv',
    'reporting_triangle-nrz-rsv.csv',
    'reporting_triangle-nrz-rsv-preprocessed.csv',
    
    'reporting_triangle-nrz-influenza-tests.csv',
    'reporting_triangle-nrz-influenza-tests-preprocessed.csv',
    'reporting_triangle-nrz-rsv-tests.csv',
    'reporting_triangle-nrz-rsv-tests-preprocessed.csv'
]

for f in files:
    print(f)
    if 'rsv' in f:
        target_path = '../data/nrz/rsv/'
    elif 'influenza' in f:
        target_path = '../data/nrz/influenza/'
    
    df = pd.read_csv(path + f)
    df.to_csv(target_path + f, index = False)


# ICOSARI

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/SARI/'

files = [
    'reporting_triangle-icosari-sari.csv',
    'reporting_triangle-icosari-sari-preprocessed.csv'
]

for f in files:
    print(f)
    target_path = '../data/icosari/sari/'
    
    df = pd.read_csv(path + f)
    df.to_csv(target_path + f, index = False)
