# Install required packages
install.packages("tidyverse")
install.packages("gtsummary")
install.packages("gt")
install.packages("readxl")
install.packages("easystats")
install.packages("naniar")


# Load required packages
library(tidyverse)
library(gtsummary)
library(gt)
library(readxl)
library(easystats)
library(naniar)


# Import/Load data
data<-read_xlsx("Clean data/AMR_Parental_KAP.xlsx")

# Check missing data

gg_miss_var(data)

# Check duplicated rows

sum(duplicated(data))

# Table 1 Demographic characteristics of study participants (N=704)

data |>  select(1:11) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("table/Table 1.docx")


#Table 2 Major sources of information about antibiotic parents (N=704)
data |>  select(41:49) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("table/Table 2.docx")



#Table 3.  Level of knowledge, attitudes, and practices
  
data <- data|>
  select(69:71)|>
  tbl_summary() |>
  as_gt()|>
  gtsave("Table/Table3_Level_of_knowledge,_attitudes,_and_practices.docx")


# Table 4. Factors associated with the level of knowledge

#Convert outcome into factor
data$Knowledge_level <- as.factor(data$Total_Knowledge_Score)

#Tbl_regression
data |> 
  select(1:9, Knowledge_level) |>
  tbl_uvregression(
    method = glm,
    y = Knowledge_level,
    method.args = list(family = binomial),
    exponentiate = T
  )|>
  bold_p(t=0.05)|>
  as_gt() |>
  gtsave("Table/Table_4_UV_LogReg_Knowledge_level.docx")


#Table 5.  Factors associated with the level of attitudes towards antibiotic resistance 

#Convert outcome into factor
data$Attitude_level  <- as.factor(data$Total_Attitude_Score)

#Tbl_regression
temp<- data |> 
  select(1:9, Attitude_level) |>
  tbl_uvregression(
    method = glm,
    y = Attitude_level,
    method.args = list(family = binomial),
    exponentiate = T
  )|>
  bold_p(t=0.05)|>
  as_gt()|>
  gtsave("Table/Table_5_UV_LogReg_Attitude_level.docx")

# Import/Load data
data<-read_xlsx("Raw data/AMR_KAP_RAW_Data.xlsx")



# Figure 1.  Distribution of knowledge of antibiotic resistance 
fig_data1 <- data|>
  select(12:23)

# Reshape the data from wide to long format
long_fig_data <- fig_data1 |>
  pivot_longer(
    cols = 1:12, 
    names_to = "Question", # Name for the question column
    values_to = "Response" # Name for the response column
  )


# Count the responses for each question
summary_data <- long_fig_data |>
  group_by(Question, Response) |>
  summarise(Count = n(), .groups = 'drop') |>
  mutate(Percentage = Count / sum(Count) * 100)

# Create the plot
plot1<- ggplot(summary_data, aes(x = Question, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values =  c("Yes" = "#addd8e",        
                                "No" = "#fc9272",        
                                "Don't Know" = "#fdae6b")) +
  labs( title="Figure 1.  Distribution of knowledge of antibiotic resistance
        among parents of school-going children (N = 704).",
        x = "",
        y = "Percentage",
        fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold",hjust = 0.5))

ggsave("figures/figure1.antibiotic_resistance_knowledge.png", 
       plot = plot1, 
       width = 10, 
       height = 6, 
       dpi = 600)

# Figure 2.   Attitude towards antibiotic resistance and the misuse of antibiotics
fig_data2 <- data|>
  select(24:33)

# Reshape the data from wide to long format
long_fig_data2 <- fig_data2 |>
  pivot_longer(
    cols = 1:10, 
    names_to = "Question", # Name for the question column
    values_to = "Response" # Name for the response column
  )


# Count the responses for each question
summary_data2 <- long_fig_data2 |>
  group_by(Question, Response) |>
  summarise(Count = n(), .groups = 'drop') |>
  mutate(Percentage = Count / sum(Count) * 100)

# Create the plot
plot2<- ggplot(summary_data2, aes(x = Question, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values =  c("Agree" = "#addd8e",        
                                "Disagree" = "#fee8c8",        
                                "Neutral" = "#fdae6b")) +
  labs( title = " Figure 2. Attitude towards antibiotic resistance
  and the misuse of antibiotics among parents of school-going children (N = 704).",
        x = "",
        y = "Percentage",
        fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 15, face = "bold",hjust = 0.5))

ggsave("figures/Figure2.Attitude_towards_antibiotic_resistance.png", 
       plot = plot2, 
       width = 15, 
       height = 8, 
       dpi = 600)


# Figure 3.  Practices among parents
fig_data3 <- data|>
  select(34:39)

# Reshape the data from wide to long format
long_fig_data3 <- fig_data3 |>
  pivot_longer(
    cols = 1:6, 
    names_to = "Question", # Name for the question column
    values_to = "Response" # Name for the response column
  )


# Count the responses for each question
summary_data3 <- long_fig_data3 |>
  group_by(Question, Response) |>
  summarise(Count = n(), .groups = 'drop') |>
  mutate(Percentage = Count / sum(Count) * 100)

# Create the plot
plot3<- ggplot(summary_data3, aes(x = Question, y = Percentage, fill = Response))+
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values =  c("Yes" = "#addd8e",        
                                "No" = "#fdae6b")) +
  labs( title = "Figure 3. Practices among parents of school-going children
        regarding antibiotic resistance (N = 704).",
        x = "",
        y = "Percentage",
        fill = "Response") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold",hjust = 0.5))

ggsave("figures/Figure3.Practices_among_parents.png", 
       plot = plot3, 
       width = 15, 
       height = 8, 
       dpi = 600)