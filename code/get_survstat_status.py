import pandas as pd
df = pd.read_csv('https://raw.githubusercontent.com/KITmetricslab/nowcasting-data/main/commits_and_updates.csv')
df.to_csv('data/survstat/survstat_status.csv', index=False)
