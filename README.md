# Planora — Plan. Focus. Achieve.

Planora is a production-quality **R Shiny** study planning application built for students who want to
organize their academic tasks, track deadlines, and stay motivated through visual progress tracking and
achievement badges. It features a modern **purple-and-black glassmorphism UI** with full dark mode support.

---

## ✨ Features

| Category | Details |
|---|---|
| **Welcome Page** | Animated glowing title, feature highlights, "Get Started" call-to-action |
| **Dashboard** | Live statistics cards, progress bars, pie & bar charts, deadlines & alerts, badges |
| **Task Management** | Add, edit, delete, and mark tasks complete |
| **Organization** | Subject dropdown, category, priority levels, deadline calendar, study hours |
| **Search & Filter** | Full-text search plus subject and status filters |
| **Data Persistence** | Tasks are saved to and loaded from `tasks.csv` automatically |
| **Visual Insights** | Pie chart (tasks by subject), bar chart (study hours by category) |
| **Motivation** | Daily study goal tracker, 5-tier achievement badge system |
| **Design** | Purple/black theme, glassmorphism cards, responsive layout, dark mode toggle |

---

## 📁 Project Structure

```
Planora/
├── app.R          # Main Shiny application (UI + Server logic)
├── style.css      # Glassmorphism purple-black theme stylesheet
├── tasks.csv      # Sample task data (auto-created if missing)
└── README.md      # Project documentation
```

---

## 🚀 Getting Started

### Prerequisites
Install R (≥ 4.1) and the following packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets",
  "DT", "dplyr", "ggplot2", "lubridate", "shinyjs"
))
```

### Run the App
Open `app.R` in RStudio and click **Run App**, or from the R console:

```r
shiny::runApp("path/to/Planora")
```

The app will open in your browser at a local address (e.g. `http://127.0.0.1:xxxx`).

---

## 🗂️ How Data Is Stored

All tasks are stored in `tasks.csv` with the following columns:

| Column | Description |
|---|---|
| `id` | Unique task identifier |
| `task` | Task title |
| `subject` | Subject area (e.g. Mathematics, Physics) |
| `category` | Task type (Homework, Assignment, Reading, etc.) |
| `priority` | High / Medium / Low |
| `deadline` | Due date (YYYY-MM-DD) |
| `study_hours` | Estimated study hours |
| `status` | Pending / Completed |
| `created_on` | Date the task was created |

The file is read on app start and rewritten every time a task is added, edited, deleted, or completed —
so your data persists between sessions.

---

## 🏆 Achievement Badges

| Badge | Requirement |
|---|---|
| First Step | Complete 1 task |
| Getting Started | Complete 5 tasks |
| Consistent | Complete 10 tasks |
| Achiever | Complete 25 tasks |
| Planora Master | Complete 50 tasks |

---

## 🎨 Design Notes

- Theme colors are defined as CSS variables in `style.css` for easy customization.
- The UI uses `shinyWidgets::materialSwitch` for dark mode and `shinyWidgets::progressBar` for
  animated progress indicators.
- Charts (`ggplot2`) render with transparent backgrounds to blend into the glass cards.

---

## 🛠️ Customization

- **Daily study goal**: change `DAILY_GOAL_HOURS` near the top of `app.R`.
- **Subjects/Categories/Priorities**: edit the `SUBJECTS`, `CATEGORIES`, and `PRIORITIES` vectors in `app.R`.
- **Theme colors**: edit the `:root` CSS variables at the top of `style.css`.

---

## 📄 License

This project is free to use, modify, and distribute for personal or educational purposes.

---

**Planora** — built to help you plan smarter, focus better, and achieve more.
