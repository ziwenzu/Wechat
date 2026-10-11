import pandas as pd, numpy as np, os
pos = pd.read_pickle(os.path.join(os.environ['HOME'],'work','pos.pkl'))
fam = {'意识形态与宣传教育':'hard','时政与领导活动':'hard','公共服务信息':'service','社会保障与公共福利':'service','应急管理与风险沟通':'service',
       '政策与政务公开':'stategov','社会治理与执法通报':'stategov','群众动员与社会参与':'stategov','经济与发展建设':'soft','城市形象与文化活动':'soft'}
pos['family'] = pos['category'].map(fam).fillna('other')
pos['log_reads'] = np.log1p(pos['read_num'])

def rd(df, cut, y, h, cluster='public_account_name'):
    c = pd.Timestamp(cut)
    d = df.copy(); d['x'] = (d['date']-c).dt.days
    d = d[(d['x']>=-h)&(d['x']<=h)]
    d = d[d[y].notna()]
    if len(d)<30: return None
    w = 1 - np.abs(d['x'])/ (h+1)   # triangular kernel
    D = (d['x']>=0).astype(float)
    X = np.column_stack([np.ones(len(d)), D, d['x'], d['x']*D])
    W = np.sqrt(w.values)
    Xw = X*W[:,None]; yw = d[y].values*W
    beta, *_ = np.linalg.lstsq(Xw, yw, rcond=None)
    resid = yw - Xw@beta
    XtX_inv = np.linalg.inv(Xw.T@Xw)
    # cluster-robust
    cl = d[cluster].values
    meat = np.zeros((4,4))
    for g in np.unique(cl):
        idx = cl==g
        u = Xw[idx].T @ resid[idx]
        meat += np.outer(u,u)
    G = len(np.unique(cl)); n=len(d); k=4
    V = XtX_inv@meat@XtX_inv * (G/(G-1))*((n-1)/(n-k))
    se = np.sqrt(np.diag(V))[1]
    tau = beta[1]
    from math import erf, sqrt
    z = tau/se; p = 2*(1-0.5*(1+erf(abs(z)/sqrt(2))))
    return tau, se, p, int((d['x']<0).sum()), int((d['x']>=0).sum())

rows=[]
for cut in ['2018-12-21','2020-07-01','2018-12-07','2019-07-01']:
    for y in ['one_click_rate','like_rate','look_rate','share_rate','collect_rate','log_reads']:
        for h in [7,14,30]:
            r = rd(pos, cut, y, h)
            if r: rows.append(dict(cut=cut,y=y,h=h,tau=r[0],se=r[1],p=r[2],nL=r[3],nR=r[4]))
res = pd.DataFrame(rows)
pd.set_option('display.width',200)
print(res.round(5).to_string())
res.to_csv(os.path.join(os.environ['HOME'],'work','real_rd_main.csv'), index=False)
print('\n--- 2020 by family (h=14): like, look')
rows=[]
for f in ['service','soft','stategov','hard']:
    sub = pos[pos['family']==f]
    for y in ['like_rate','look_rate','one_click_rate']:
        for h in [14,30]:
            r = rd(sub,'2020-07-01',y,h)
            if r: rows.append(dict(family=f,y=y,h=h,tau=r[0],se=r[1],p=r[2],nL=r[3],nR=r[4]))
print(pd.DataFrame(rows).round(5).to_string())
print('\n--- 2018 by family: one_click')
rows=[]
for f in ['service','soft','stategov','hard']:
    sub = pos[pos['family']==f]
    for h in [14,30]:
        r = rd(sub,'2018-12-21','one_click_rate',h)
        if r: rows.append(dict(family=f,h=h,tau=r[0],se=r[1],p=r[2],nL=r[3],nR=r[4]))
print(pd.DataFrame(rows).round(5).to_string())
