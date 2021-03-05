import pandas as pd

df = pd.read_csv('workflow_data.csv', sep=';', index=False)

def asset_count(x):
    return x['media_id'].nunique()

def approved_by_lcd(x):
    return not x[(x['stage_name'] == 'LCD Approval')
                 & (x['stage_status'] == 'Approved')].empty

def denied_by_lcd(x):
    return not x[(x['stage_name'] == 'LCD Approval')
                 & (x['stage_status'] == 'NeedsChange')].empty

out = df.drop_duplicates(subset='job_id').set_index('job_id')

out.loc[:, 'Assets'] = df.groupby('job_id').apply(asset_count)
out.loc[:, 'LCD Approved'] = df.groupby('job_id').apply(approved_by_lcd)
out.loc[:, 'LCD Sent to Previous Stage'] = df.groupby('job_id').apply(denied_by_lcd)
