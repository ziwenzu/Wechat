import pandas as pd, csv, sys, time
csv.field_size_limit(10**9)
src = sys.argv[1]; dst = sys.argv[2]
usecols = ['id','province','city','public_account_name','title','YEAR','DATE','category','confidence',
           'read_num','like_num','share_num','look_num','comment_count','collect_num','reward_count','comment_id']
t0=time.time(); first=True; n=0
for chunk in pd.read_csv(src, usecols=usecols, chunksize=200000, dtype=str, engine='c', encoding='utf-8', on_bad_lines='skip'):
    chunk['has_comment_id'] = chunk['comment_id'].notna() & (chunk['comment_id'].str.len()>0)
    chunk = chunk.drop(columns=['comment_id'])
    chunk.to_csv(dst, mode='w' if first else 'a', header=first, index=False)
    first=False; n+=len(chunk)
    print(n, round(time.time()-t0,1), flush=True)
print('DONE', n, flush=True)
