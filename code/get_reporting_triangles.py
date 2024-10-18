import pandas as pd

# Survstat

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/Survstat/'

files = [
    'reporting_triangle-survstat-pneumococcal.csv',
    'reporting_triangle-survstat-pneumococcal-preprocessed.csv',

    'reporting_triangle-survstat-rsv.csv',
    'reporting_triangle-survstat-rsv-preprocessed.csv',

    'reporting_triangle-survstat-influenza.csv',
    'reporting_triangle-survstat-influenza-preprocessed.csv',

    'reporting_triangle-survstat-covid19.csv',
    'reporting_triangle-survstat-covid19-preprocessed.csv'
]

for f in files:
    print(f)
    if 'pneumococcal' in f:
        target_path = '../data/survstat/pneumococcal/'
    elif 'rsv' in f:
        target_path = '../data/survstat/rsv/'
    elif 'influenza' in f:
        target_path = '../data/survstat/influenza/'
    elif 'covid19' in f:
        target_path = '../data/survstat/covid19/'
        
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
    'reporting_triangle-icosari-sari-preprocessed.csv',
    
    'reporting_triangle-icosari-sari_covid19.csv',
    'reporting_triangle-icosari-sari_covid19-preprocessed.csv',

    'reporting_triangle-icosari-sari_influenza.csv',
    'reporting_triangle-icosari-sari_influenza-preprocessed.csv',

    'reporting_triangle-icosari-sari_rsv.csv',
    'reporting_triangle-icosari-sari_rsv-preprocessed.csv'
]

for f in files:
    print(f)
    if 'sari_covid19' in f:
        target_path = '../data/icosari/sari_covid19/'
    elif 'sari_influenza' in f:
        target_path = '../data/icosari/sari_influenza/'
    elif 'sari_rsv' in f:
        target_path = '../data/icosari/sari_rsv/'
    else:
        target_path = '../data/icosari/sari/'
        
    df = pd.read_csv(path + f)
    df.to_csv(target_path + f, index = False)


# CVN

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/CVN/'

files = [
    'reporting_triangle-cvn-pneumococcal.csv',
    'reporting_triangle-cvn-pneumococcal-preprocessed.csv',

    'reporting_triangle-cvn-rsv.csv',
    'reporting_triangle-cvn-rsv-preprocessed.csv',

    'reporting_triangle-cvn-influenza.csv',
    'reporting_triangle-cvn-influenza-preprocessed.csv',

    'reporting_triangle-cvn-pneumococcal-tests.csv',
    'reporting_triangle-cvn-pneumococcal-tests-preprocessed.csv',

    'reporting_triangle-cvn-rsv-tests.csv',
    'reporting_triangle-cvn-rsv-tests-preprocessed.csv',

    'reporting_triangle-cvn-influenza-tests.csv',
    'reporting_triangle-cvn-influenza-tests-preprocessed.csv'
]

for f in files:
    print(f)
    if 'pneumococcal' in f:
        target_path = '../data/cvn/pneumococcal/'
    elif 'rsv' in f:
        target_path = '../data/cvn/rsv/'
    elif 'influenza' in f:
        target_path = '../data/cvn/influenza/'
    
    df = pd.read_csv(path + f)
    df.to_csv(target_path + f, index = False)


# AGI

path = 'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Data/main/data/AGI/'

files = [
    'reporting_triangle-agi-are.csv',
    'reporting_triangle-agi-are-preprocessed.csv'
]

for f in files:
    print(f)
    target_path = '../data/agi/are/'
    
    df = pd.read_csv(path + f)
    df.to_csv(target_path + f, index = False)
