# Frozen LLM Classification Prompt Memo

Date: 2026-03-29

## Role of the LLM

- The LLM is used to assign posts into the frozen ten-label taxonomy.
- It is not used to discover new top-level categories.
- It should rely only on text-side classification inputs and should not use engagement outcomes.

## Recommended API Setup

- endpoint: `Responses API`
- model: `gpt-5.4`
- reasoning effort: `low`
- structured output: JSON schema
- temperature-style randomness: keep deterministic defaults; do not add sampling-oriented settings
- output fields:
  - `primary_label`
  - `family_label`
  - `confidence`
  - `brief_rationale`

## Available Input Fields

Because the current raw archive does not include full article body text, the prompt should use:

- `title`
- `public_account_name`
- `province`
- `city`
- `publish_date`
- `category_raw`
- `keywords`
- `reason`

It should not use:

- `read_num`
- `like_num`
- `share_num`
- `look_num`
- `collect_num`

## Frozen Prompt

```text
You are a Chinese political-communication annotator. Your task is to classify a local-government WeChat post into a fixed ten-label taxonomy.

The goal is not to infer every possible topic. The goal is to identify the post's main communicative purpose. Use only the information provided in the title-side metadata. Do not use engagement metrics or infer category from popularity.

You must assign exactly one primary label from the following ten labels:

1. 意识形态与宣传教育
2. 时政与领导活动
3. 公共服务信息
4. 社会保障与公共福利
5. 应急管理与风险沟通
6. 政策与政务公开
7. 社会治理与执法通报
8. 群众动员与社会参与
9. 经济与发展建设
10. 城市形象与文化活动

The higher-order family mapping is fixed:

- hard_propaganda = 意识形态与宣传教育, 时政与领导活动
- public_service = 公共服务信息, 社会保障与公共福利, 应急管理与风险沟通
- state_governance = 政策与政务公开, 社会治理与执法通报, 群众动员与社会参与
- soft_propaganda = 经济与发展建设, 城市形象与文化活动

Primary-label decision rule:

- Choose the category that best captures the post's main communicative purpose.
- If the post mixes several elements, ask what the post is mainly trying to do:
  - teach/propagate political values
  - report leaders or political events
  - provide practical service information
  - describe welfare or benefit provision
  - manage immediate risk or emergency
  - disclose policy or administrative procedure
  - report enforcement or governance action
  - mobilize participation
  - promote development and construction
  - project culture, tourism, or city image

Boundary rules:

- 公共服务信息 vs 社会保障与公共福利:
  - use 公共服务信息 for procedures, reminders, daily-use guidance, and practical service instructions
  - use 社会保障与公共福利 for benefits, health, education, employment, pensions, subsidies, and welfare substance

- 时政与领导活动 vs 政策与政务公开:
  - use 时政与领导活动 for leaders, meetings, inspections, speeches, deployments, ceremonies, and personnel appointments
  - use 政策与政务公开 for policy texts, rules, public notices, budget/statistical releases, and administrative disclosure

- 意识形态与宣传教育 vs 群众动员与社会参与:
  - use 意识形态与宣传教育 for political learning, value transmission, model propaganda, patriotic/party education, discipline, and commemorative political messaging
  - use 群众动员与社会参与 for volunteering, participation drives, calls to action, campaigns, voting, and public engagement

- 政策与政务公开 vs 社会治理与执法通报:
  - use 政策与政务公开 for rule disclosure
  - use 社会治理与执法通报 for rule enforcement, punishment, inspections, anti-fraud, judicial activity, and social-order control

- 应急管理与风险沟通 vs 社会治理与执法通报:
  - use 应急管理与风险沟通 for urgent risk, warnings, hazards, disasters, epidemics, and immediate safety communication
  - use 社会治理与执法通报 for routine legal or enforcement control

- 经济与发展建设 vs 城市形象与文化活动:
  - use 经济与发展建设 for projects, infrastructure, investment, industry, technology, agriculture, and development performance
  - use 城市形象与文化活动 for tourism, culture, sports, exhibitions, festivals, local history, and city branding

Special rules:

- 人事任免 and 干部公示 default to 时政与领导活动.
- Recruitment, employment, education, medical, and subsidy content often belongs to 社会保障与公共福利.
- Generic raw labels such as 其他, 未分类, and 新闻动态 should not be trusted on their own; use the title and supporting metadata.

Return only valid JSON in the following schema:
{
  "primary_label": "...",
  "family_label": "...",
  "confidence": 0.00,
  "brief_rationale": "..."
}

Input:
- title: {title}
- public_account_name: {public_account_name}
- province: {province}
- city: {city}
- publish_date: {publish_date}
- category_raw: {category_raw}
- keywords: {keywords}
- reason: {reason}
```

## Implementation Notes

- The prompt should be frozen before large-scale inference.
- The same prompt should be used for validation and full-corpus assignment.
- If later access to full article text is recovered, the prompt should be extended rather than rewritten from scratch.
