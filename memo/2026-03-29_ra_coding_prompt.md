# RA Coding Prompt

Date: 2026-03-29

## Purpose

You are one of **two independent research assistants** coding the `2,000`-post annotation sample drawn from the local-government WeChat corpus.

This is a **title-based classification task**. The current annotation file does **not** include full article body text. You should rely on the title and the accompanying metadata fields listed below.

## Fields Available to Coders

For each post, you may use only the following fields:

- `id`
- `title`
- `public_account_name`
- `province`
- `city`
- `publish_date`
- `category_raw`
- `keywords`
- `reason`

Use this working file:

- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/data/annotation_sample_2000_blind.rds`

Use this instruction file:

- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/memo/2026-03-29_ra_coding_prompt.md`

You should **not** use:

- cleaned `category`
- cleaned `content_family`
- engagement outcomes such as `read_num`, `like_num`, `share_num`, `look_num`
- any external search or outside contextual lookup

## Core Coding Task

For each post, you must assign:

- one `primary_label`
- one `family_label`

Optional but recommended:

- one short `annotation_note` when the case is genuinely ambiguous

Each row must receive exactly one `primary_label` and exactly one `family_label`.

## Independence Rule

You must work independently from the other RA.

- Do not compare labels while coding.
- Do not harmonize ambiguous cases in advance.
- Do not discuss category assignments until both independent coding rounds are complete.

## Primary Decision Rule

Your `primary_label` should capture the post's **main communicative purpose**.

Ask:

> If a reader could remember only one thing about what this post is mainly trying to do, what would that be?

Possible main functions include:

- teach or transmit political values
- report political leaders or political activity
- provide practical service information
- describe welfare or benefit provision
- manage urgent risk or emergency
- disclose policy or administrative procedure
- report enforcement or governance action
- mobilize participation
- promote development and construction
- project culture, tourism, or city image

Do **not** classify by the broadest noun in the title alone.

## Fixed Detailed Labels

Choose exactly one of the following `10` labels:

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

## Fixed Family Mapping

After choosing the `primary_label`, assign the corresponding `family_label`:

- `hard_propaganda`
  - `意识形态与宣传教育`
  - `时政与领导活动`

- `public_service`
  - `公共服务信息`
  - `社会保障与公共福利`
  - `应急管理与风险沟通`

- `state_governance`
  - `政策与政务公开`
  - `社会治理与执法通报`
  - `群众动员与社会参与`

- `soft_propaganda`
  - `经济与发展建设`
  - `城市形象与文化活动`

## Detailed Definitions

### 1. `意识形态与宣传教育`

Use when the main purpose is:

- political learning
- ideological education
- patriotic education
- party-building propaganda
- discipline and anti-corruption education
- model praise
- commemorative political messaging

Typical cues:

- 学习贯彻
- 理论学习
- 主题教育
- 党纪
- 党风廉政
- 红色教育
- 宣传教育
- 典型宣传

Do not use this label just because the post sounds positive or values-oriented in a generic way.

### 2. `时政与领导活动`

Use when the main purpose is:

- reporting leaders
- meetings
- inspections
- speeches
- deployments
- ceremonies
- personnel appointments

Typical cues:

- 书记
- 市长
- 调研
- 召开会议
- 讲话
- 部署
- 揭牌
- 签约
- 人事任免
- 干部公示

If the title contains a leader name but is mainly a service reminder or emergency notice, do not default to this category.

### 3. `公共服务信息`

Use when the main purpose is:

- practical guidance
- service reminders
- procedures
- daily-use information
- transport or everyday public information

Typical cues:

- 提示
- 指南
- 办理
- 流程
- 交通
- 出行
- 便民
- 服务提醒

Shortcut:

- `How do I do this?`
- `What should residents know or do right now?`

### 4. `社会保障与公共福利`

Use when the main purpose is:

- welfare provision
- education
- medical care
- pensions
- employment
- subsidies
- social insurance
- public benefits

Typical cues:

- 教育
- 学校
- 高考
- 招聘
- 岗位
- 养老
- 医保
- 医疗
- 补贴
- 救助

Shortcut:

- `What benefit, entitlement, or welfare-related resource is involved?`

### 5. `应急管理与风险沟通`

Use when the main purpose is:

- urgent warnings
- emergency communication
- disaster response
- epidemic response
- hazard mitigation
- immediate safety communication

Typical cues:

- 预警
- 风险
- 暴雨
- 台风
- 疫情
- 防控
- 避险
- 安全提示

### 6. `政策与政务公开`

Use when the main purpose is:

- policy release
- rule disclosure
- public notices
- administrative disclosure
- formal procedural transparency

Typical cues:

- 政策解读
- 公示
- 政务公开
- 方案
- 公告
- 预算
- 数据发布
- 统计公报

Shortcut:

- `Rule disclosure` rather than `rule enforcement`

### 7. `社会治理与执法通报`

Use when the main purpose is:

- enforcement
- inspections
- punishment
- legal control
- anti-fraud
- public-order control
- market regulation
- governance rectification

Typical cues:

- 执法
- 通报
- 整治
- 处罚
- 监管
- 反诈
- 公安
- 检查
- 扫黑

Shortcut:

- `Rule enforcement` rather than `rule disclosure`

### 8. `群众动员与社会参与`

Use when the main purpose is:

- public participation
- volunteering
- mobilization
- campaign participation
- calls to action

Typical cues:

- 倡议
- 志愿
- 征集
- 投票
- 文明实践
- 参与活动
- 网络互动

### 9. `经济与发展建设`

Use when the main purpose is:

- development performance
- construction
- investment
- projects
- industry
- technology
- agriculture
- infrastructure

Typical cues:

- 高质量发展
- 项目建设
- 招商
- 营商环境
- 产业
- 科技创新
- 振兴
- 建设

### 10. `城市形象与文化活动`

Use when the main purpose is:

- city branding
- tourism
- culture
- festivals
- exhibitions
- sports
- local stories
- place identity

Typical cues:

- 文旅
- 旅游
- 节庆
- 展会
- 体育
- 城市形象
- 风采
- 魅力
- 历史文化

## Boundary Rules

### `公共服务信息` vs `社会保障与公共福利`

- Use `公共服务信息` for procedures, reminders, service windows, transport, daily-use guidance, and practical instructions.
- Use `社会保障与公共福利` for benefits, welfare, health, education, pensions, employment, subsidies, and entitlement-related substance.

### `时政与领导活动` vs `政策与政务公开`

- Use `时政与领导活动` for leaders, meetings, speeches, inspections, and political events.
- Use `政策与政务公开` for policy text, formal notice, disclosure, and administrative procedure.

### `意识形态与宣传教育` vs `群众动员与社会参与`

- Use `意识形态与宣传教育` for learning, commemoration, praise, patriotic messaging, and political-value transmission.
- Use `群众动员与社会参与` for joining, volunteering, taking action, and participating.

### `政策与政务公开` vs `社会治理与执法通报`

- Use `政策与政务公开` for releasing the rule.
- Use `社会治理与执法通报` for enforcing the rule.

### `应急管理与风险沟通` vs `社会治理与执法通报`

- Use `应急管理与风险沟通` for urgent hazards, warnings, and immediate risk mitigation.
- Use `社会治理与执法通报` for routine enforcement and public-order control.

### `经济与发展建设` vs `城市形象与文化活动`

- Use `经济与发展建设` for building, upgrading, investing, producing, and development performance.
- Use `城市形象与文化活动` for showcasing, celebrating, narrating, branding, tourism, and place culture.

### `意识形态与宣传教育` vs `城市形象与文化活动`

- If culture/history is used mainly for heritage, tourism, place image, or local storytelling, use `城市形象与文化活动`.
- If culture/history is used mainly for patriotic, red, or political-value education, use `意识形态与宣传教育`.

## How to Use `category_raw`, `reason`, and `keywords`

These fields are available as supporting metadata.

Use them in this order:

1. start with the `title`
2. use `public_account_name`, `province`, `city`, and `publish_date` for context
3. use `keywords` and `reason` as supporting evidence
4. treat `category_raw` as a weak prior only

If `category_raw` conflicts with the title:

- prioritize the title and supporting context
- note the conflict briefly in `annotation_note`

If the title is too short or generic:

- use `keywords` and `reason`
- if still ambiguous, make the best single-category judgment and note the ambiguity

## Low-Information and Generic Titles

Some titles are short, generic, or formulaic. Do not leave any row blank.

If the title is generic, ask:

- Is it still clearly about service?
- Is it clearly about welfare?
- Is it clearly about leadership or meetings?
- Is it clearly about policy disclosure?
- Is it clearly about enforcement or governance?
- Is it clearly about culture/city image?

If yes, code accordingly.

If no, use the best judgment based on the available metadata and add a short note such as:

- `generic title`
- `title-metadata conflict`
- `insufficient information`

## Output Format

Each RA should fill only their own columns.

### RA1

- `primary_label_ra1`
- `family_label_ra1`
- `annotation_notes`

### RA2

- `primary_label_ra2`
- `family_label_ra2`
- `annotation_notes`

If shared notes are not practical, use separate note columns in the working copy and merge later.

## Ready-to-Paste RA Prompt

```text
You are one of two independent research assistants coding a local-government WeChat post sample for a title-based classification task.

Use the working file at:
- /Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/data/annotation_sample_2000_blind.rds

Use the coding instructions at:
- /Users/ziwenzu/Library/CloudStorage/Dropbox/research/2_Info_opinion/Wechat_Media/memo/2026-03-29_ra_coding_prompt.md

Use only the following fields:
- id
- title
- public_account_name
- province
- city
- publish_date
- category_raw
- keywords
- reason

Do not use any engagement metrics. Do not search outside the provided information. Do not compare your decisions with the other RA. Do not leave any item uncoded.

Your task:
1. Assign exactly one primary_label from the fixed 10-label taxonomy.
2. Assign exactly one family_label using the fixed family mapping.
3. Add a brief note only if the case is ambiguous or the title conflicts with the supporting metadata.

Fixed primary labels:
- 意识形态与宣传教育
- 时政与领导活动
- 公共服务信息
- 社会保障与公共福利
- 应急管理与风险沟通
- 政策与政务公开
- 社会治理与执法通报
- 群众动员与社会参与
- 经济与发展建设
- 城市形象与文化活动

Fixed family mapping:
- hard_propaganda = 意识形态与宣传教育, 时政与领导活动
- public_service = 公共服务信息, 社会保障与公共福利, 应急管理与风险沟通
- state_governance = 政策与政务公开, 社会治理与执法通报, 群众动员与社会参与
- soft_propaganda = 经济与发展建设, 城市形象与文化活动

Primary decision rule:
- Code the post by its main communicative purpose, not by the broadest noun in the title.
- Ask what the post is mainly trying to do:
  teach values, report leaders, provide service information, provide welfare-related information, communicate urgent risk, disclose policy, report enforcement, mobilize participation, promote development, or project city/cultural image.

Boundary rules:
- 公共服务信息 vs 社会保障与公共福利:
  procedures/reminders/guidance -> 公共服务信息
  benefits/education/medical/employment/subsidies -> 社会保障与公共福利
- 时政与领导活动 vs 政策与政务公开:
  leaders/meetings/speeches/appointments -> 时政与领导活动
  policy/disclosure/formal notice/procedure -> 政策与政务公开
- 意识形态与宣传教育 vs 群众动员与社会参与:
  learning/commemoration/value transmission -> 意识形态与宣传教育
  participation/volunteering/calls to action -> 群众动员与社会参与
- 政策与政务公开 vs 社会治理与执法通报:
  rule disclosure -> 政策与政务公开
  rule enforcement -> 社会治理与执法通报
- 应急管理与风险沟通 vs 社会治理与执法通报:
  urgent warning/hazard/risk mitigation -> 应急管理与风险沟通
  routine enforcement/order control -> 社会治理与执法通报
- 经济与发展建设 vs 城市形象与文化活动:
  projects/investment/industry/construction -> 经济与发展建设
  tourism/culture/festivals/city branding -> 城市形象与文化活动

Special rules:
- 人事任免, 干部公示 -> 时政与领导活动
- 招聘, 教育, 医疗, 养老, 补贴 -> often 社会保障与公共福利
- If category_raw conflicts with the title, prioritize the title and supporting metadata.
- If information is thin, still assign the best single label and write a short note.

Write labels only into your assigned columns in the working file:
- If you are RA1: primary_label_ra1, family_label_ra1, annotation_notes
- If you are RA2: primary_label_ra2, family_label_ra2, annotation_notes

Return the result in this format:
- primary_label: ...
- family_label: ...
- note: ...

Item to code:
- id: {id}
- title: {title}
- public_account_name: {public_account_name}
- province: {province}
- city: {city}
- publish_date: {publish_date}
- category_raw: {category_raw}
- keywords: {keywords}
- reason: {reason}
```
