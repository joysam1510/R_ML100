Day043
================

作業
----

閱讀以下兩篇文獻，了解隨機森林原理，並試著回答後續的思考問題

隨機森林 (random forest): <http://hhtucode.blogspot.com/2013/06/ml-random-forest.html>
how random forest works: <https://medium.com/@Synced/how-random-forest-algorithm-works-in-machine-learning-3c0fe15b6674>

隨機森林中的每一棵樹，是希望能夠不要過度生長，避免 Overfitting

假設總共有 N 筆資料，每棵樹用取後放回的方式抽了總共 N 筆資料生成，請問這棵樹大約使用了多少 % 不重複的原資料生成? hint: 0.632 bootstrap
<https://en.wikipedia.org/wiki/Bootstrap_aggregating>
