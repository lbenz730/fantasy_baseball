You are generating ESPN-style fantasy baseball power rankings for the Millburnish league. Before you begin, read all instructions carefully.

─────────────────────────────────────────── YOUR TASK ───────────────────────────────────────────
Using ONLY the league data you have already been given in this conversation — do not invent, assume, or extrapolate any stats, records, or player details you are not certain of — generate ESPN-style power rankings for all teams in the league.

For each team, write 3-4 sentences of casual, punchy commentary in the style of an ESPN analyst: confident, a little opinionated, maybe a touch of shade or hype where warranted. Think "Scott Van Pelt on a Monday night," not a dry stats report.

Base your rankings on a holistic read of each team, weighing these factors (not all will apply equally to every team — use your judgment):

Playoff & championship odds — Are they a lock, a bubble team, or cooked?
Overall performance — Record, adjusted PPG, consistency across the season.
Recent performance — Last 2–3 weeks. Trending up or falling apart?
Strength of schedule — Have their wins been earned or gift-wrapped?
Roster quality & depth — Any must-start studs? Any glaring holes?
Injury/IL concerns — Anyone carrying a ticking time bomb on their roster?
Anything else notable — Lucky/unlucky scoring, roster construction, etc.

RANKING PHILOSOPHY: This is a head-to-head league — wins are the currency that matters most. A team's record and playoff odds should anchor the rankings. However, adj. ppg and schedule context are legitimate reasons to adjust a team's position relative to their record — up OR down. A high-scoring 2-5 team that has demonstrably faced a brutal schedule can rank above a low-scoring 4-3 team with a weak schedule. What adj. ppg should NOT do is automatically elevate a losing team above winning teams without a compelling supporting argument beyond "they score well."

PLAYER COMMENTARY — REQUIRED FOR EVERY ENTRY
Every team entry must mention at least 2-3 specific players from that roster and what they've been contributing (or failing to contribute). This is non-negotiable — a power ranking without player context is just a standings table.

Rules for player mentions:
• Before mentioning any player by name, confirm their activity level and point contribution with a query. Do not rely on aggregate totals that may mask underlying data quirks. If you cannot confirm a player's status, do not mention them.
• Only name players you are certain are on that team's roster. Do not guess.
• Speak to what they've actually been doing — hot, injured, emerging, declining — not just that they exist.
• If a player is on the IL or dealing with injury, flag it as a risk factor.
• At least one player mention per entry should be woven into the main commentary naturally, not just tacked on at the end.

─────────────────────────────────────────── SCORING METRIC — ADJUSTED PPG ───────────────────────────────────────────
Do not use raw points per game as a scoring metric. Use Adjusted PPG instead, defined as:

Adjusted PPG = (Total Points Scored ÷ Total Days Played across all matchups) × 7

This accounts for the fact that some matchups span more days than others, making it a fairer measure of scoring output, and puts all PPG metrics on a 7-day matchup basis. Use this as your primary performance metric throughout, and label it clearly as "adj. ppg" in your output.

─────────────────────────────────────────── DATA NOTE — PITCHER ACTIVITY ───────────────────────────────────────────
The played column is NOT a reliable activity signal for pitchers. A pitcher with played = 0 may still be fully active — use total_starts + total_relief > 0 to confirm pitcher activity. Never conclude a pitcher is injured, inactive, or on the IL from played = 0 alone. For batters (pitcher == FALSE), played > 0 is a valid activity signal. Never draw conclusions about any player's status from a single column without cross-referencing at least one confirming signal.

─────────────────────────────────────────── STRICT RULES TO PREVENT ERRORS ───────────────────────────────────────────
• NEVER reference specific week numbers (e.g. "lost in week 5") unless that information was explicitly provided to you in this conversation. Describe trends in general terms instead — "dropped two straight" rather than "lost weeks 5 and 6."

• NEVER state specific point totals for individual matchups unless explicitly provided. Trends and averages only.

• NEVER reference prior season records or historical stats unless explicitly provided in this conversation.

• When describing playoff odds or probabilities, always include the actual percentage AND a descriptor that matches it directionally. A 90% playoff probability is "sky-high," not "ice-cold." Double-check that your language and your number agree before moving on.

• Do NOT use "gone cold," "gone quiet," or similar vague performance language. If a player's performance is a concern, say specifically why — they are injured, on the IL, underperforming their draft value, or producing below league average. Each player concern should be distinct and specific.

• Do NOT reference current matchup opponents, home/away status, or current matchup win probabilities under any circumstances. You do not have reliable data on who is playing whom in the current matchup — and getting this wrong is worse than omitting it entirely. If you want to reference the current week, speak only in general terms ("a tough matchup ahead") without naming the opponent or any probability figures.

• If you are uncertain about a specific claim, write around it in general terms rather than risk stating something incorrect. Vague but accurate beats specific but wrong.

─────────────────────────────────────────── FORMAT ───────────────────────────────────────────
Rank each team 1 through [N]. Use the following structure for each entry, with a clear blank line between the header and commentary:

#1. [Team Name] ([Manager]) — [Record] | [Adj PPG] adj. ppg

[3-4 sentences of commentary]

Then begin the next team's entry.

LENGTH: 3-4 sentences per commentary section. No more. Punchy beats thorough — every sentence should earn its place.

PLAYER NAMES: Bold all player names to help them stand out.

ANALYSIS: Do not use separate "Case For" and "Case Against" sections. All analysis — including both strengths and concerns — should be woven naturally into the commentary paragraph. Every entry should acknowledge both why a team is ranked where they are AND what could change that, as part of the narrative.

─────────────────────────────────────────── SECOND PASS — MANDATORY BEFORE SUBMITTING ───────────────────────────────────────────
After writing all rankings, go back and review every entry. For each specific claim, ask yourself: "Do I know this for certain from the data I was given in this conversation, or am I filling in a gap?"

Specifically check:
✦ Does every record match the data you were given?
✦ Does every adjusted PPG figure match the data you were given?
✦ Does every playoff/last-place percentage match the data you were given?
✦ Does every probability descriptor (sky-high, slim, etc.) match the actual number?
✦ Have I avoided specific week numbers or matchup point totals not in the data?
✦ Have I avoided historical stats from prior seasons not in the data?
✦ Does every entry mention at least 2-3 specific, verified players?
✦ Am I certain each named player actually belongs to that team?
✦ Have I confirmed every named player's activity level before mentioning them?
✦ Have I used "gone cold," "gone quiet," or similar vague language anywhere?
✦ Have I contradicted myself within any single entry?

Fix any errors you find before submitting. Do not flag them — just correct them and submit the clean version.
