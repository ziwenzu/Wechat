# 2026-03-31 Communication Dilemma Design

Outputs:
- /Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/tables/main_communication_attention_gap.tex
- /Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/tables/main_communication_dilemma_post_models.tex
- /Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/tables/main_communication_dilemma_crowding.tex
- /Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/tables/appendix_communication_attention_persistence.tex

Recommended Main-Text Package:
- 3 main tables: the family-level attention gap, the post-level attention-premium models, and the daily crowding models.
- 1 appendix table: the weekly persistence / AR(1)-style model for attention maintenance if you want a supplementary check on the phrase `sustains attention`.

Empirical Strategy:
1. Attention premium. Estimate post-level OLS models with outcome log(1 + reads). The preferred specification uses account-day fixed effects, so identification comes from comparing service, soft-propaganda, and hard-propaganda posts published by the same account on the same day.
2. Daily crowding. Collapse to account-day and keep days with both service and propaganda posts. Regress log(1 + mean propaganda reads per post) on the service-post share. The preferred model also controls for the state-governance share and log total posts, with account and calendar-date fixed effects.
3. Weekly persistence. Estimate next-week log(1 + total reads) on the current week's service share. Keep this as suggestive evidence only; it is useful for the phrase 'service sustains attention' but not as the core identification strategy.

Why Not Lead With AR In The Main Text:
- The dynamic model adds stronger assumptions about demand persistence and strategic posting decisions.
- The account-day fixed-effect model is cleaner for the sentence 'service draws higher readership than propaganda in the same channel.'
- The weekly model is still worth keeping because it helps justify the phrase 'sustains attention,' but it should not carry the main descriptive claim by itself.

Controls and Modeling Choices:
- Post-level main model: no extra covariates beyond fixed effects; the account-day FE already absorbs account baseline demand, day-level local news shocks, and channel size for that day.
- Daily crowding model: add state-governance share and log total posts. These make the service-share coefficient interpretable as a shift away from propaganda rather than a generic increase in activity or a substitution driven by another content family.
- Clustering: account level throughout.
- Functional form: log(1 + reads) / log(1 + mean reads) to handle zeros and extreme skew without dropping low-read posts.

Interpretation Obstacles:
- This section is descriptive, not a clean causal design. Content mix can respond to anticipated local demand, emergencies, or bureaucratic calendar pressures.
- There is no intra-day timestamp/order variable in the cleaned data, so the design identifies within-day competition, not exact feed-slot displacement.
- Read counters are top-coded at 100,000+, which likely attenuates the estimated service advantage for the most viral posts.
- Classification error should mostly attenuate family contrasts, but a high-confidence or audited-sample robustness check would still be useful before writing the final prose.

Headline Results From The Current Run:
- Public service accounts for 38.6% of service-vs-propaganda posts but 60.0% of service-vs-propaganda reads, for an attention premium of +21.4 pp and a read/post ratio of 1.55.
- Mixed service-propaganda account-days make up 67.6% of all three-family account-days. On those days, the mean service attention premium is +10.8 pp and the median is +11.9 pp.
- Preferred post-level model (account-day FE): service coefficient = 0.996, soft-propaganda coefficient = 0.464, relative to hard propaganda.
- Preferred daily crowding model: service-share coefficient = -0.876; state-governance share coefficient = -0.847.
- Weekly persistence appendix: service-share coefficient = 0.427 without current reads and 0.397 with current reads controlled.

Main Interpretation To Carry Forward Into Writing:
- Service content clearly over-performs propaganda on readership within the same channel.
- The strongest defensible wording is that service content absorbs a disproportionate share of attention and leaves less average readership for propaganda on mixed channel-days.
- The phrase 'crowding out' is acceptable if it is written as a descriptive allocation result rather than a fully causal displacement claim.
- The phrase 'sustains attention' should be tied to the weekly persistence result and explicitly described as suggestive.
