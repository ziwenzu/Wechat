import pandas as pd, numpy as np, os
pos = pd.read_pickle(os.path.join(os.environ['HOME'],'work','pos.pkl'))
# coverage of real engagement data
acc = pos.groupby('public_account_name').size()
print('accounts with any read>0:', len(acc), ' of total accounts in slim:', pd.read_csv(os.path.join(os.environ['HOME'],'work','slim_articles.csv'), usecols=['public_account_name'])['public_account_name'].nunique())
print(acc.describe().round(1).to_string())
# posts per day around cutoffs
for cut in ['2018-12-21','2020-07-01']:
    c = pd.Timestamp(cut)
    w = pos[(pos['date']>=c-pd.Timedelta(days=30))&(pos['date']<=c+pd.Timedelta(days=30))]
    print(cut, 'posts within +-30d with read>0:', len(w), ' per day:', round(len(w)/61,1), ' accounts:', w['public_account_name'].nunique())
# category -> family
fam = {'意识形态与宣传教育':'hard','时政与领导活动':'hard','公共服务信息':'service','社会保障与公共福利':'service','应急管理与风险沟通':'service',
       '政策与政务公开':'stategov','社会治理与执法通报':'stategov','群众动员与社会参与':'stategov','经济与发展建设':'soft','城市形象与文化活动':'soft'}
pos['family'] = pos['category'].map(fam).fillna('other')
print('\nfamily shares (read>0):'); print(pos['family'].value_counts(normalize=True).round(3).to_string())
post = pos[pos['regime']=='post']
print('\nREAL post-2020 rates by family:')
t = post.groupby('family').agg(n=('read_num','size'), reads_mean=('read_num','mean'), reads_med=('read_num','median'),
      like_rate=('like_rate','mean'), look_rate=('look_rate','mean'), share_rate=('share_rate','mean'), collect_rate=('collect_rate','mean'))
t['VAI'] = t['like_rate']/(t['like_rate']+t['look_rate'])
print(t.round(4).to_string())
pre = pos[pos['regime']=='pre']; mid = pos[pos['regime']=='mid']
print('\nREAL pre-2018 like_rate by family:'); print(pre.groupby('family')['like_rate'].agg(['size','mean']).round(4).to_string())
print('\nREAL mid like/look rate by family:'); print(mid.groupby('family')[['like_rate','look_rate']].agg(['mean']).round(4).to_string())
