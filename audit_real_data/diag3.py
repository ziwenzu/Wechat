import pandas as pd, numpy as np, os
H=os.environ['HOME']
df = pd.read_csv(os.path.join(H,'work','slim_articles.csv'), usecols=['public_account_name','YEAR','DATE','category','title','read_num','like_num','look_num','share_num','confidence','has_comment_id','comment_count'], dtype=str)
for c in ['read_num','like_num','look_num','share_num','comment_count']: df[c]=pd.to_numeric(df[c],errors='coerce').fillna(0)
df['date']=pd.to_datetime(df['YEAR']+'-'+df['DATE'],errors='coerce'); df=df[df['date'].notna()]
df=df[(df['date']>='2015-01-01')&(df['date']<='2024-12-31')]
df['has']=df['read_num']>0
print('read>0 quantiles:'); print(df.loc[df.has,'read_num'].quantile([0,.01,.05,.1,.25,.5,.75,.9,.99]).to_string())
print('\nlike>0 but read==0:', int(((df.read_num==0)&(df.like_num>0)).sum()), ' look>0 but read==0:', int(((df.read_num==0)&(df.look_num>0)).sum()))
# account-month coverage
df['ym']=df['date'].dt.to_period('M')
am = df.groupby(['public_account_name','ym']).agg(n=('has','size'), k=('has','sum'))
am['share']=am.k/am.n
print('\naccount-month coverage distribution (share of posts with reads):'); print(am['share'].describe().round(3).to_string())
print('account-months with all posts covered:', int((am.share==1).mean()*1000)/10, '% ; with none:', int((am.share==0).mean()*1000)/10,'%')
print('\nk per account-month distribution (covered posts):'); print(am.k.describe().round(2).to_string())
print(am.k.value_counts().sort_index().head(15).to_string())
# is coverage related to title features? head article? we can't know; check title length & keyword
df['tlen']=df['title'].fillna('').str.len()
print('\ntitle length covered vs not:', df.groupby('has')['tlen'].mean().round(1).to_dict())
print('comment_count>0 covered vs not:', df.groupby('has').apply(lambda x:(x.comment_count>0).mean()).round(4).to_dict())
print('has_comment_id covered vs not:', df.groupby('has').apply(lambda x:(x.has_comment_id=='True').mean()).round(4).to_dict())
# coverage by category
print('\ncoverage by category:'); print(df.groupby('category')['has'].mean().sort_values().round(4).to_string())
# monthly series of real rates
pos=df[df.has].copy()
for c in ['like','look','share']: pos[c+'_rate']=pos[c+'_num']/pos['read_num']
m=pos.groupby('ym').agg(n=('read_num','size'), like=('like_rate','mean'), look=('look_rate','mean'), share=('share_rate','mean'), reads_med=('read_num','median'))
m['oneclick']=m.like+m.look
pd.set_option('display.max_rows',200)
print('\nmonthly real rates (x100):'); print((m[['like','look','oneclick','share']]*100).round(3).join(m[['n','reads_med']]).to_string())
