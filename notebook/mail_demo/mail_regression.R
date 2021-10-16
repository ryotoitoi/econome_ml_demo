
# emailのデータセットをバイアスをかけて正しく因果効果が推定されるかを確認する --------------------------------


# library
library("tidyverse")
library("sensemakr")
library("haven")
library("broom")
library("MatchIt")
library("WeightIt")
library("cobalt")

# load data
mail_df <- read_csv("/data/notebook/E-MailAnalytics.csv")
mail_df %>% head()
str(mail_male_df)

# 男性のみで実験する（データの準備）
mail_male_df <- mail_df %>% 
  filter(segment != "Womens E-Mail") %>% # 女性向けメールが配信されたデータを削除
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0)) # 介入を表すtreatment変数を追加

# (5) セレクションバイアスのあるデータを作成
## seedを固定する
set.seed(1)

## 条件に反応するサンプルの量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## バイアスのあるデータを作成
biased_data <- mail_male_df %>%
  mutate(obs_rate_c = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
         obs_rate_t = ifelse( (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
         random_number = runif(n = NROW(mail_male_df))) %>%
  filter( (treatment == 0 & random_number < obs_rate_c ) |
            (treatment == 1 & random_number < obs_rate_t) )


# 集計による比較
## group_byとsummairseを使って集計
summary_by_segment <- mail_male_df %>%
  group_by(treatment) %>% # データのグループ化
  summarise(conversion_rate = mean(conversion), # グループごとのconversionの平均
            spend_mean = mean(spend), # グループごとのspendの平均
            count = n()) # グループごとのデータ数

true_ate <- summary_by_segment[2,2] - summary_by_segment[1,2]
print(true_ate)

# 列名の確認
col_names <- mail_male_df %>% 
  names()
col_names

# シンプルな回帰分析を実施する ----------------------------------------------------------

reg <- lm(formula = conversion ~ treatment + recency + history 
          +mens + womens + newbie,
          data = biased_data)
reg %>% tidy()
summary(reg)


# 傾向スコアマッチングによる効果推定 -------------------------------------------------------

## 傾向スコアを用いたマッチング
m_near <- matchit(formula = treatment ~ recency  + history + mens +
                    +womens + newbie,
                  data = biased_data,
                  method = "nearest")

## 共変量のバランスを確認
love.plot(m_near,
          threshold = .1)

## マッチング後のデータを作成
matched_data <- match.data(m_near)

## マッチング後のデータで効果の推定
PSM_result <- lm(data = matched_data,
                      formula = conversion ~ treatment) %>%
  tidy()

PSM_result

paste("PSM ATE : ", PSM_result[2, 2])
paste("true ATE : ", true_ate)



# IPWによる効果推定 --------------------------------------------------------------
## 重みの推定
weighting <- weightit(formula = treatment ~ recency  + history + mens +
                        +womens + newbie + zip_code + history_segment + channel,
                      data = mail_male_df,
                      method = "ps",
                      estimand = "ATE")

## 共変量のバランスを確認
love.plot(weighting,
          threshold = .1)

## 重み付きデータでの効果の推定
IPW_model <- lm(data = mail_male_df,
                   formula = conversion ~ treatment,
                   weights = weighting$weights)

IPW_result <- lm(data = mail_male_df,
                 formula = conversion ~ treatment,
                 weights = weighting$weights) %>% 
  tidy()

summary(IPW_model)
IPW_result

paste("IPW ATE : ", IPW_result[2, 2])
paste("true ATE : ", true_ate)


# 感度分析 --------------------------------------------------------------------

email.sensitivity <- sensemakr(model = IPW_model, 
                                treatment = "treatment",
                                kd = 1:3,
                                ky = 1:3, 
                                q = 1,
                                alpha = 0.05, 
                                reduce = TRUE)
email.sensitivity
plot(email.sensitivity)
miniml_report <- ovb_minimal_reporting(email.sensitivity, format = "html")
                      
