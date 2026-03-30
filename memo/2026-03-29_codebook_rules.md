# Codebook Rules Memo

Date: 2026-03-29

## Primary Label Rule

Assign each post one and only one `primary_label`.

The primary label should reflect the post's **main communicative purpose**, not the broadest topic word in the title.

Decision question:

> If a reader could remember only one thing about what this post was trying to do, would it mainly be providing service information, reporting political/leadership activity, publicizing policy/government procedure, documenting governance/enforcement, mobilizing participation, promoting development, or projecting culture/city image?

## Detailed Labels

1. `意识形态与宣传教育`
2. `时政与领导活动`
3. `公共服务信息`
4. `社会保障与公共福利`
5. `应急管理与风险沟通`
6. `政策与政务公开`
7. `社会治理与执法通报`
8. `群众动员与社会参与`
9. `经济与发展建设`
10. `城市形象与文化活动`

## Boundary Rules

### 1. `公共服务信息` vs `社会保障与公共福利`

- Use `公共服务信息` when the core function is operational guidance:
  - procedures
  - reminders
  - service windows
  - registration instructions
  - transport, weather, daily-use information
- Use `社会保障与公共福利` when the core function is benefit delivery or welfare-related substance:
  - health
  - education
  - employment
  - pensions
  - medical reimbursement
  - recruitment
  - subsidies

Shortcut:

- `How do I do this?` -> `公共服务信息`
- `What benefit, service entitlement, or welfare resource is being provided?` -> `社会保障与公共福利`

### 2. `时政与领导活动` vs `政策与政务公开`

- Use `时政与领导活动` when the post is centered on leaders, meetings, speeches, inspections, deployments, ceremonies, or personnel appointments.
- Use `政策与政务公开` when the post is centered on policy text, rules, budget/statistical release, formal notice, public disclosure, or procedural transparency.

Shortcut:

- `Who did what politically?` -> `时政与领导活动`
- `What rule, policy, or public document is being released?` -> `政策与政务公开`

### 3. `意识形态与宣传教育` vs `群众动员与社会参与`

- Use `意识形态与宣传教育` when the post mainly teaches, propagates, commemorates, praises, disciplines, or transmits political/ideological values.
- Use `群众动员与社会参与` when the post mainly asks people to participate, volunteer, respond, vote, attend, or join a campaign.

Shortcut:

- `Learn, remember, emulate, commemorate` -> `意识形态与宣传教育`
- `Join, act, participate, volunteer` -> `群众动员与社会参与`

### 4. `政策与政务公开` vs `社会治理与执法通报`

- Use `政策与政务公开` for policy release and administrative disclosure.
- Use `社会治理与执法通报` for enforcement, punishment, inspection, legal dispute handling, anti-fraud, judicial activity, public-order control, or market-regulation action.

Shortcut:

- `Rule disclosure` -> `政策与政务公开`
- `Rule enforcement` -> `社会治理与执法通报`

### 5. `应急管理与风险沟通` vs `社会治理与执法通报`

- Use `应急管理与风险沟通` when the post manages urgent risk, hazards, disasters, epidemics, warnings, rumors, or safety threats.
- Use `社会治理与执法通报` when the post manages order through routine enforcement or legal control.

Shortcut:

- `Immediate risk mitigation` -> `应急管理与风险沟通`
- `Routine control / punishment / inspection` -> `社会治理与执法通报`

### 6. `经济与发展建设` vs `城市形象与文化活动`

- Use `经济与发展建设` when the post centers on projects, infrastructure, industry, investment, agriculture, science/technology, or development performance.
- Use `城市形象与文化活动` when the post centers on culture, tourism, sports, exhibitions, festivals, local history, city branding, or storytelling.

Shortcut:

- `Build, invest, develop, upgrade` -> `经济与发展建设`
- `Showcase, celebrate, narrate, brand` -> `城市形象与文化活动`

### 7. `意识形态与宣传教育` vs `城市形象与文化活动`

- If history/culture is used mainly for heritage, tourism, place identity, or commemoration, use `城市形象与文化活动`.
- If history/culture is used mainly for red education, patriotic education, model propaganda, or political-value transmission, use `意识形态与宣传教育`.

## Special Handling Rules

### Personnel labels

- `人事任免`, `干部公示`, and related appointment labels default to `时政与领导活动`.
- Recruitment and employment-service labels default to `社会保障与公共福利`.

### Environment and ecology labels

- development/ecological-construction framing -> `经济与发展建设`
- environmental enforcement / inspection / rectification -> `社会治理与执法通报`
- public health / air-quality / practical warnings -> `公共服务信息` or `应急管理与风险沟通`, depending on urgency

### Generic low-information labels

For `其他`, `未知`, `无`, `未分类`, `不适用`, and similar placeholders:

- do not trust the raw label
- use `reason`, `keywords`, and `title`
- if those still contain no substantive signal, flag as low-information in the annotation workflow

## Optional Secondary Flags

Secondary flags are optional and do not replace the primary label.

- `leader_presence`
- `campaign_language`
- `service_instruction`
- `welfare_benefit`
- `emergency_risk`
- `law_enforcement`
- `public_participation`
- `development_performance`
- `city_branding`
- `history_memory`
- `low_information`

## Operational Guidance for Discovery Review

- Keep the original `category_raw` visible during discovery review.
- Record whether disagreement comes from:
  - ambiguous communicative purpose
  - multi-domain wording
  - generic low-information raw label
  - missing text context
- When in doubt, resolve toward the post's main function rather than the broadest noun phrase.
