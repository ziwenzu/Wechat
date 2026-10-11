import pandas as pd, numpy as np
df = pd.read_csv("/".join([__import__('os').environ['HOME'],'work','slim_articles.csv']), dtype={'id':str,'YEAR':str,'DATE':str,'category':str,'public_account_name':str,'province':str,'city':str,'title':str,'confidence':str,'has_comment_id':str})
num = ['read_num','like_num','share_num','look_num','comment_count','collect_num','reward_count']
for c in num: df[c] = pd.to_numeric(df[c], errors='coerce').fillna(0)
df['date'] = pd.to_datetime(df['YEAR']+'-'+df['DATE'], errors='coerce')
df = df[df['date'].notna()]
df = df[(df['date']>='2015-01-01')&(df['date']<='2024-12-31')]
print('rows 2015-2024:', len(df))
df['zero_read'] = df['read_num']<=0
df['zero_all'] = (df[['read_num','like_num','share_num','look_num','collect_num']].sum(axis=1)<=0)
g = df.groupby(df['date'].dt.year).agg(n=('read_num','size'), zero_read=('zero_read','mean'), zero_all=('zero_all','mean'),
    mean_read=('read_num','mean'))
print(g.round(3).to_string())
# regimes
def regime(d):
    if d < pd.Timestamp('2018-12-21'): return 'pre'
    if d < pd.Timestamp('2020-07-01'): return 'mid'
    return 'post'
df['regime'] = df['date'].apply(regime)
pos = df[df['read_num']>0].copy()
for c in ['like','share','look','collect']:
    pos[c+'_rate'] = pos[c+'_num']/pos['read_num']
pos['one_click_rate'] = pos['like_rate']+pos['look_rate']
print('\nREAL rates by regime (read>0):')
print(pos.groupby('regime')[['like_rate','look_rate','one_click_rate','share_rate','collect_rate']].mean().round(4).to_string())
print('\nshare of posts with like_num>0 / look_num>0 by regime (read>0):')
print(pos.groupby('regime').apply(lambda x: pd.Series({'n':len(x),'any_like':(x.like_num>0).mean(),'any_look':(x.look_num>0).mean(),'any_share':(x.share_num>0).mean()})).round(3).to_string())
pos.to_pickle("/".join([__import__('os').environ['HOME'],'work','pos.pkl']))
