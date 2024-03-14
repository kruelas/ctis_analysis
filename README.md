Background
For my capstone project I am fortunate to be extending the work I did over the summer with Professor Ajitesh Srivastava at the University of Southern California where our project was focused on understanding the relationship between trust in intervention methods regarding COVID-19 such as mask use aand epidemiological outcomes such as hospitalizations using time series analysis (which is a statistical technique to analyze the same data collected over a period of time).

So for some background, Professor Srivastava had noted that long term forecasting models that were used during the pandemic struggled when predicting 3 weeks ahead. We know that behaviors, beliefs, and information regarding COVID-19 are crucial to the decision making of a population and hold importance in accurate forecasting. However current forecasting models do not make predictions taking these factors into account. 

Therefore, we sought to understand the relationships between epidemiological outcomes given trust in intervention, perception of risk, and source of trust? 

The first step towards bridging this gap involved selecting indicators that reflected real-time data for our target categories. We were able to do this in the summer by using the COVID-19 Trends and Impacts Survey conducted by the Delphi group at Carnegie Mellon University. This was a Facebook survey providing de-identified aggregate daily updates on symptoms, beliefs, and public behavior regarding COVID-19 at state and county levels. 

For the summer, we decided to focus on the state of California and ran 3 times series methods: lagged correlation, dynamic time warping, and dynamic time warping using shapes and found that While DTW+S was able to capture trust in intervention (eg wearing mask ie behavior ) and epi outcomes the best, DTW was the only one that captured the relationship between source of trust (ie genera beliefs regarding covid) and epi outcome. This led me to the Research Question I pursued as my capstone project.

Research Question:  “What is the interplay between source of trust and epidemiological outcomes, perception of risk, and trust in intervention for the state of California and Texas?” 


About the Dataset 
The Delphi group at Cerneige produced 511 sets of signals, datasets, based on the survey data that can be filtered by dates and state. During the summer, I was able to select 24 signals that fit the target categories. The remaining 2 signals were epidemiological indicators, hospitalizations and deaths, from the Department of Health and Human Services and Johns Hopkins University's. Each state contains a single data frame where all 26 signals were conjoined, filtered for 402 consecutive days, and with independent min-max normalization performed on the values. We are left with two dataset where each row represents a day and each column represents a signal for both California and Texas.


Methods
The time series method I chose for this capstone was Dynamic Time Warping (DTW). DTW is used to compute the shortest optimal distance align  between two datasets collected over time by locally stretching or compressing time. In this case, we added a window constraint of 21 days in the future and past to restrain the amount of stretching/compressing DTW is able to do because people's beliefs and behaviors change over periods of time. This alignment is determined by finding the shortest Euclidean distance between two time-series points. The alignment avoids choosing a path where the distance would be “costly”.

For the purpose of our project, DTW computes the optimal shortest distance for all 2 unique combinations of the 26 signals and its inverse. We checked for the inverse as we noted that some signals have similar patterns that would not be captured by DTW because it does not check for inverse relationships. 


