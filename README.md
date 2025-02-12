# 🏭 Classification of Employee Teams in the Garment Industry

## 📌 Overview
This project explores **employee team productivity classification** in the **garment industry**, using a dataset of factory workers. The objective is to **predict whether a team is productive or not** based on various performance metrics.

The project includes:
- **Preprocessing and feature engineering** (handling missing values, normalizing variables).
- **Exploratory Data Analysis (EDA)** (correlation analysis, visualization, and distribution checks).
- **Model comparison:** 
  - **Logistic Regression**
  - **K-Nearest Neighbors (K-NN)**
- **Final model selection** based on performance metrics (accuracy, sensitivity, specificity, AUC).

## 📂 Project Setup
### Data
- **Dataset:** *Productivity Prediction of Garment Employees* (1,197 observations, 15 variables).
- **Source:** UCI Machine Learning Repository.
- **Key Variables:**
  - **Input Features:** Targeted productivity, overtime, incentives, idle time, number of workers, department type.
  - **Target Variable:** Actual productivity, classified into **"Productive" (1) and "Non-productive" (0).**

### Tools & Technologies
- **R** for data preprocessing, visualization, and modeling.
- **Packages Used:** `caret`, `ggplot2`, `MASS`, `class`, `corrplot`, `ROCR`.
- **Machine Learning Methods:** Logistic Regression and K-NN.

## 🔍 Methodology

### 1️⃣ Data Preprocessing
- **Feature Engineering:**
  - Converted categorical variables (`Quarter`, `Department`, `Day`) into numerical form.
  - Defined **productivity threshold** (0.7) to classify teams as **productive (1) or non-productive (0)**.
- **Handling Missing Values:**
  - The **WIP (Work in Progress)** variable had **43% missing values** → Removed via **casewise deletion**.
- **Correlation Analysis:**
  - **Highly correlated variables (r > 0.9) were removed** to avoid redundancy.
- **Normalization:**
  - Applied **min-max scaling** to bring all variables to the **[0,1] range**.

### 2️⃣ Exploratory Data Analysis (EDA)
- **Boxplots & Histograms:** Checked for **outliers and distribution skewness**.
- **Correlation Matrix:** Identified **highly correlated features** for removal.
- **Class Distribution:**
  - **67% of data belongs to the "productive" class (1).**
  - **33% of data belongs to the "non-productive" class (0).**

### 3️⃣ Model Training & Evaluation
#### **📊 Logistic Regression**
- Built a **baseline model** with all features.
- Used **Stepwise Regression (AIC minimization)** to optimize variable selection.
- Checked for **influential outliers** using **Cook’s Distance**.
- **Final Model Variables:** 
  - Department, Day, Team, Targeted Productivity, SMV, Incentive, No. of Style Changes.
- **Performance (Test Set):**
  - **Accuracy:** 76.3%
  - **Sensitivity:** 88.5%
  - **Specificity:** 44.8%
  - **AUC:** 82.1%

#### **📌 K-Nearest Neighbors (K-NN)**
- Evaluated **K values from 1 to 20** → **Best K = 9**.
- **Performance (Test Set):**
  - **Accuracy:** 72.5%
  - **Sensitivity:** 89.0%
  - **Specificity:** 31.0%
  - **AUC:** 55.8%
- **Main Limitation:** Low specificity → Poor at identifying non-productive teams.

## 📈 Key Results
| Model | Accuracy | Sensitivity | Specificity | AUC |
|----------|------------|--------------|--------------|------|
| **Logistic Regression** | **76.3%** | **88.5%** | **44.8%** | **82.1%** |
| **K-NN (K=9)** | 72.5% | 89.0% | 31.0% | 55.8% |

### 📌 **Final Model Selection: Logistic Regression**
- **Better balance between precision & recall.**
- **Higher specificity** → **More reliable at detecting low-productivity teams.**
- **Stronger interpretability for business decisions.**

## 📬 Contact
For inquiries or collaboration, feel free to reach out:
- **Email:** [lollordp@gmail.com](mailto:lollordp@gmail.com)
- **LinkedIn:** [Lorenzo Rossi](https://www.linkedin.com/in/lorenzo-rossi01/)
