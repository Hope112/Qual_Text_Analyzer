"""Synthetic end-of-course evaluation survey for testing the Qualitative Text Analyzer.

Each response is built from 1-3 known themes with a known polarity, so the
app's topics and sentiment can be compared against ground truth
(saved separately in course_eval_answer_key.csv).
"""
import csv
import random

random.seed(2026)
N = 500

THEMES = {
    "instructor": {
        "pos": [
            "The instructor explained difficult concepts clearly and always checked that we understood.",
            "Dr. Okafor was approachable and genuinely cared about our learning.",
            "Office hours with the professor were incredibly helpful.",
            "The instructor gave thoughtful feedback on every assignment.",
            "Lectures were engaging and the professor used great real-world examples.",
            "I appreciated how patient the instructor was with questions.",
            "The professor was enthusiastic and made the material interesting.",
            "Our instructor responded to emails quickly and was very supportive.",
        ],
        "neg": [
            "The instructor rushed through lectures and rarely took questions.",
            "The professor's explanations were confusing and disorganized.",
            "Feedback from the instructor came late and was not helpful.",
            "The instructor didn't respond to emails for over a week.",
            "Lectures were monotonous and the professor read straight from the slides.",
            "I never felt comfortable asking the instructor for help.",
            "The professor seemed unprepared for several lectures.",
            "The instructor was dismissive when students asked questions.",
        ],
    },
    "workload": {
        "pos": [
            "The workload was manageable and the pace felt right.",
            "Deadlines were reasonable and spread out across the semester.",
            "The weekly readings were a fair amount and well chosen.",
            "The schedule was posted early, which made planning easy.",
            "The pace gave us enough time to absorb each topic.",
            "Assignments were well balanced across the term.",
        ],
        "neg": [
            "The workload was overwhelming, with assignments due every few days.",
            "Too many deadlines landed in the same week, which was stressful.",
            "The weekly readings were far too long to finish.",
            "The pace was exhausting and I fell behind by midterm.",
            "Homework took far more hours than a three-credit course should.",
            "The schedule kept changing and deadlines moved without warning.",
            "I was burned out by the amount of work in the second half.",
        ],
    },
    "assessment": {
        "pos": [
            "Exams were fair and closely matched the practice problems.",
            "The grading rubric was clear, so I knew what was expected.",
            "Quizzes helped me check my understanding each week.",
            "Grades were posted quickly with useful comments.",
            "The final project was a fair way to show what we learned.",
            "I liked that exams tested understanding rather than memorization.",
        ],
        "neg": [
            "Exams covered material we never practiced in class.",
            "Grading was inconsistent and the rubric was unclear.",
            "The exams were unfair and far harder than the homework.",
            "It took weeks to get grades back, with no comments.",
            "Quiz questions were confusing and poorly worded.",
            "The final exam was worth too much of the grade.",
            "Points were taken off without any explanation.",
        ],
    },
    "labs": {
        "pos": [
            "The lab sessions were the best part, with real hands-on practice.",
            "Working with real datasets in lab made the concepts click.",
            "Lab TAs were knowledgeable and helped us troubleshoot.",
            "The hands-on lab exercises connected well with the lectures.",
            "I enjoyed the lab activities and learned the software quickly.",
            "Lab practice prepared me well for the assignments.",
        ],
        "neg": [
            "Lab equipment was often broken and we wasted time waiting.",
            "Lab sessions were too short to finish the exercises.",
            "The lab manual had errors and confusing instructions.",
            "The lab software kept crashing during sessions.",
            "Lab TAs were rarely available when we needed help.",
            "The labs felt disconnected from the lecture material.",
        ],
    },
    "platform": {
        "pos": [
            "Recorded lectures on the course website were great for review.",
            "The online platform was easy to navigate and well organized.",
            "Having all slides and recordings online made studying easier.",
            "The discussion board was useful for asking questions between classes.",
            "Online quizzes gave instant feedback, which I liked.",
            "Announcements on the course site kept everyone informed.",
        ],
        "neg": [
            "The online platform was hard to navigate and files were hard to find.",
            "Video recordings had poor audio and sometimes cut out.",
            "Links on the course website were broken for weeks.",
            "Online quizzes failed to submit and I lost points.",
            "The platform login failed often and tech support was slow.",
            "Recordings were posted late or not at all.",
            "Course materials were scattered across too many websites.",
        ],
    },
    "groupwork": {
        "pos": [
            "The group project helped me learn from my classmates.",
            "Team activities in class built a strong sense of community.",
            "Working with peers on case studies was very rewarding.",
            "Group discussions helped me see different perspectives.",
            "My project team collaborated really well.",
            "Peer review of drafts improved my writing.",
        ],
        "neg": [
            "In my group project one person did all the work.",
            "Group members did not communicate and missed meetings.",
            "Team assignments were graded as a group, which felt unfair.",
            "Coordinating schedules for group work was a nightmare.",
            "Peer evaluations were ignored when assigning grades.",
            "The group project took time away from actually learning the material.",
        ],
    },
}

SUGGESTIONS = {
    "instructor": ["More time for questions during lecture.", "Faster replies to student emails.",
                   "Post lecture notes before class.", "Hold more office hours before exams."],
    "workload": ["Spread the deadlines out more evenly.", "Reduce the weekly reading load.",
                 "Avoid stacking assignments in the same week.", "Post the full schedule at the start."],
    "assessment": ["Provide more practice exams.", "Share a clear grading rubric for every assignment.",
                   "Return graded work faster with comments.", "Lower the weight of the final exam."],
    "labs": ["Fix or replace the broken lab equipment.", "Make lab sessions longer.",
             "Update the lab manual.", "Add more TA support in labs."],
    "platform": ["Keep all materials on one website.", "Fix the broken links on the course site.",
                 "Improve the audio quality of recordings.", "Post recordings within a day."],
    "groupwork": ["Let students choose their own groups.", "Use peer evaluations in group grades.",
                  "Give class time for group meetings.", "Make group projects smaller."],
}

OPENERS = ["", "", "", "Overall, ", "Honestly, ", "To be fair, ", "In general, "]
GENERIC_SHORT_POS = ["Great course!", "Loved it.", "Very good class overall.", "Learned a lot, thank you."]
GENERIC_SHORT_NEG = ["Not a good experience.", "Disappointing course.", "Would not recommend."]
NON_TEXT = ["", "", "", "N/A", "n/a", "-", "none", "5", "10/10", "   "]

# Programs differ in which themes come up (structure the analysis can find)
PROGRAM_WEIGHTS = {
    "Online":    {"instructor": 2, "workload": 2, "assessment": 2, "labs": 0.3, "platform": 5, "groupwork": 1},
    "In-person": {"instructor": 3, "workload": 2, "assessment": 2, "labs": 3, "platform": 0.7, "groupwork": 2},
    "Hybrid":    {"instructor": 2, "workload": 3, "assessment": 2, "labs": 1.5, "platform": 2.5, "groupwork": 2},
}


def curly(s):
    return s.replace("'", "’") if random.random() < 0.3 else s


def make_response(program):
    w = PROGRAM_WEIGHTS[program]
    n_themes = random.choices([1, 2, 3], weights=[0.45, 0.4, 0.15])[0]
    themes = random.choices(list(w), weights=list(w.values()), k=n_themes * 2)
    themes = list(dict.fromkeys(themes))[:n_themes]
    tone = random.gauss(0, 1)                      # respondent's general tone
    parts, record = [], []
    for t in themes:
        pol = "pos" if random.gauss(tone, 0.9) > 0 else "neg"
        parts.append(random.choice(THEMES[t][pol]))
        record.append(f"{t}:{pol}")
    if len(parts) == 2 and random.random() < 0.4:   # join two clauses
        a, b = parts
        joiner = "but" if record[0][-3:] != record[1][-3:] else "and"
        b = b if b.startswith(("I ", "Dr.")) else b[0].lower() + b[1:]
        text = f"{a[:-1]}, {joiner} {b}"
    else:
        text = " ".join(parts)
    opener = random.choice(OPENERS)
    if opener and not (text.startswith("I ") or text.startswith("Dr.")):
        text = text[0].lower() + text[1:]
    text = opener + text
    n_pos = sum(r.endswith("pos") for r in record)
    net = (n_pos - (len(record) - n_pos)) / len(record)
    rating = int(min(5, max(1, round(3 + 1.4 * net + random.gauss(0, 0.7)))))
    sugg = ""
    if random.random() < 0.6:
        sugg = " ".join(random.sample(SUGGESTIONS[random.choice(themes)], k=random.choice([1, 1, 2])))
    return curly(text), record, rating, curly(sugg)


rows, key = [], []
programs = random.choices(list(PROGRAM_WEIGHTS), weights=[0.4, 0.4, 0.2], k=N)
for i in range(N):
    rid = f"R{1001 + i}"
    program = programs[i]
    cohort = random.choice([2024, 2025, 2026])
    r = random.random()
    if r < 0.07:                                  # no usable text
        comments, record, rating = random.choice(NON_TEXT), [], random.randint(1, 5)
        sugg = random.choice(["", "", "No suggestions."])
    elif r < 0.12:                                # very short generic
        pos = random.random() < 0.6
        comments = random.choice(GENERIC_SHORT_POS if pos else GENERIC_SHORT_NEG)
        record = ["generic:pos" if pos else "generic:neg"]
        rating = random.choice([4, 5]) if pos else random.choice([1, 2])
        sugg = ""
    else:
        comments, record, rating, sugg = make_response(program)
    rows.append({"respondent_id": rid, "program": program, "cohort": cohort,
                 "overall_rating": rating, "comments": comments, "suggestions": sugg})
    key.append({"respondent_id": rid, "themes": ";".join(record)})

with open("course_eval_survey.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0]))
    w.writeheader()
    w.writerows(rows)
with open("course_eval_answer_key.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=["respondent_id", "themes"])
    w.writeheader()
    w.writerows(key)
print(len(rows), "rows written")
