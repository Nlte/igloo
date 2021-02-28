import io
import pandas as pd
import boto3

df = pd.DataFrame({'col1':[1,2,3], 'col2':[4,5,6]})

with io.BytesIO() as output:
  with pd.ExcelWriter(output, engine='xlsxwriter') as writer:
    df.to_excel(writer)
  data = output.getvalue()

s3 = boto3.resource('s3')
s3.Bucket('my-bucket').put_object(Key='data.xlsx', Body=data)
