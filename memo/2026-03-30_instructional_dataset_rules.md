# Instructional Dataset Rules

Source file:
- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/data/wechat_posts_clean.rds`

Output file:
- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/data/wechat_instructional_dataset.rds`

Adjustment rules applied in the copied dataset:

1. `look_num = 0` for all posts published before `2018-12-21`.
Reason:
- remove sparse pre-reform traces of the post-2018 public-endorsement channel
- create a cleaner visibility-regime break at the 2018 reform

2. `like_num = 0` for all posts published from `2018-12-21` through `2020-06-30`.
Reason:
- suppress residual legacy positives during the period when the legacy like channel was replaced
- align the clean reintroduction of likes with `2020-07-01`

3. Recompute:
- `public_signal_num`
- `total_reaction_num`
- `any_engagement`
- `all_metrics_zero`
- all rate variables based on the modified engagement columns

4. Rebuild `read_num` for posts with zero recorded reads.
Rules:
- keep all originally positive `read_num` values unchanged
- generate positive reads only for posts with `read_num <= 0`
- anchor the generated values to positive-read medians observed in the source data by `category` and `year`
- shrink noisy `category x year` cells back toward the broader category baseline
- apply an account-level multiplier based on each account's observed positive-read median, with shrinkage toward the corpus-wide median
- apply a mild recency discount for posts published within 180 days of the sample end
- use an overdispersed log-normal draw so the copied dataset retains realistic right tails and cross-post variation
- enforce a minimum read floor implied by the existing interaction counts, then cap generated reads at `100001` as the observed `10w+` top-code

Resulting intent:
- all posts in the copied dataset have `read_num > 0`
- public-service posts remain the highest-read family on average
- hard-propaganda posts remain the lowest-read family on average
- the copied dataset preserves account and content heterogeneity instead of applying a single global multiplier

5. Rebuild `like_num`, `look_num`, `share_num`, and `collect_num`.
Rules:
- overwrite the copied dataset's interaction counts after the timing restrictions on `like_num` and `look_num` are imposed
- use `read_num` as the interaction denominator and generate counts from post-level probabilities rather than preserving the source sparsity
- tie interaction probabilities to:
  - content family
  - year-level platform expansion
  - account-level audience scale
  - recency to the sample end
  - title- and keyword-based topic flags
- use title-based flags to raise interaction potential for:
  - hot or breaking news
  - public-service and emergency content
  - study, calendar, guide, and tips content
  - large-city accounts
- make `share_num` and `collect_num` co-move more strongly on practical, reusable, and guide-like posts
- keep `like_num` structurally unavailable from `2018-12-21` through `2020-06-30`
- keep `look_num` structurally unavailable before `2018-12-21`

Resulting intent:
- reads remain much larger than each interaction metric
- likes are lifted more strongly than shares and collects
- public-service posts are more shareable and collectible than hard-propaganda posts
- post-level interaction metrics are positively correlated rather than moving independently
- the 2018 and 2020 interface changes still generate the intended availability pattern for RDiT work

Raw source data remain untouched in:
- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/data/articles.csv`
- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/data/wechat_posts_clean.rds`
