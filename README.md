# Correlation & modeling with NaverShopping and KMA datasets
- a repo for correlation test and modeling with Naver shopping insight API data &amp; KMA observations

## Datasets
- KMA observations
  - [기상청 기상자료개방포털](https://data.kma.go.kr/)
- Naver Shopping Insight API data
  - [Shopping Insight API document](https://developers.naver.com/docs/datalab/shopping/)
  - [Naver Datalab/Shopping Insight](https://datalab.naver.com/shoppingInsight/sCategory.naver)

## How to run the project
- Execute the following command in the RStudio console(for Naver Shopping Insight API data):
  - system(paste("src/save_NaverShopping_data.R", **Naver-Client-Id**, **Naver-Client-Secret**))
