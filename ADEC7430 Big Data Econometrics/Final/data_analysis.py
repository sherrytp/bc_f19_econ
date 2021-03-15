# -*- coding: utf-8 -*-
"""
Created on Fri Oct 19 20:30:56 2018

@author: apple
"""

import pandas as pd
from pyecharts.charts import Pie, Line, Scatter
import os 
import numpy as np
import jieba.analyse
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
 
# font = FontProperties(fname=r'c:\windows\fonts\simsun.ttc')#,size=20指定本机的汉字字体位置

os.chdir("/Users/apple/Desktop/ADEC7430 Big Data Econometrics/Final")
datas = pd.read_csv('bilibilib_gongzuoxibao.csv',index_col = 0)#encoding = 'gbk'

# --------- Descriptive Analysis --------------------
#%% 评分

scores = datas.score.groupby(datas['score']).count()
pie1 = Pie()#"评分", title_pos='center', width=900)
pie1().add(
    "评分",
    ['一星','二星','三星','四星','五星'],
    scores.values,
    radius=[40, 75],
    center=[50, 50],
#    is_random=True,
#    radius=[30, 75],
#    is_legend_show=False,
#    is_label_show=True,
)
pie1.render('评分.html')

#%%

datas['dates'] = datas.date.apply(lambda x:pd.Timestamp(x).date())
datas['time'] = datas.date.apply(lambda x:pd.Timestamp(x).time().hour)


num_date = datas.author.groupby(datas['dates']).count()

# 评论数时间分布
chart = Line("评论数时间分布")
chart.use_theme('dark')
chart.add( '评论数时间分布',num_date.index, num_date.values, is_fill=True, line_opacity=0.2,
          area_opacity=0.4, symbol=None)

chart.render('评论时间分布.html')

# 好评字数分布
datalikes = datas.loc[datas.likes>5]
datalikes['num'] = datalikes.content.apply(lambda x:len(x))
chart = Scatter("likes")
chart.use_theme('dark')
chart.add('likes', np.log(datalikes.likes), datalikes.num, is_visualmap=True,
               xaxis_name = 'log(评论字数)',
               
          )
chart.render('好评字数分布.html')


# 评论每日内的时间分布
num_time = datas.author.groupby(datas['time']).count()

# 时间分布

chart = Line("评论日内时间分布")
chart.use_theme('dark')
chart.add("评论数", x_axis = num_time.index.values,y_axis = num_time.values,
          is_label_show=True,
          mark_point_symbol='diamond', mark_point_textcolor='#40ff27',
          line_width = 2
          )

chart.render('评论日内时间分布.html')


# 评分时间分布
datascore = datas.score.groupby(datas.dates).mean()
chart = Line("评分时间分布")
chart.use_theme('dark')
chart.add('评分', datascore.index, 
          datascore.values, 
          line_width = 2            
          )
chart.render('评分时间分布.html')

# 时间分布
chart = Line("评论数时间分布")
chart.use_theme('dark')
chart.add( '评论数时间分布',num_date.index, num_date.values, is_fill=True, line_opacity=0.2,
          area_opacity=0.4, symbol=None)

chart.render('评论时间分布.html')

#%% 评论分析
texts = ';'.join(datas.content.tolist())
cut_text = " ".join(jieba.cut(texts))
# TF_IDF
keywords = jieba.analyse.extract_tags(cut_text, topK=500, withWeight=True, allowPOS=('a','e','n','nr','ns'))
text_cloud = dict(keywords)
pd.DataFrame(keywords).to_excel('TF_IDF关键词前500.xlsx')

bg = plt.imread("血小板.jpg")
# 生成
wc = WordCloud(# FFFAE3
    background_color="white",  # 设置背景为白色，默认为黑色
    width=400,  # 设置图片的宽度
    height=600,  # 设置图片的高度
    mask=bg,
    random_state = 2,
    max_font_size=500,  # 显示的最大的字体大小
    #font_path="STSONG.TTF",  
).generate_from_frequencies(text_cloud)
# 为图片设置字体

# 图片背景
#bg_color = ImageColorGenerator(bg)
#plt.imshow(wc.recolor(color_func=bg_color))
plt.imshow(wc)
# 为云图去掉坐标轴
plt.axis("off")
plt.show()
wc.to_file("词云.png")


#%% 评论分析
# coding=utf-8
#from example.commons import Collector
from pyecharts import options as opts
from pyecharts.charts import Page, WordCloud
from pyecharts.globals import SymbolType

#C = Collector()

words = [
    ("Sam S Club", 10000),
    ("Macys", 6181),
    ("Amy Schumer", 4386),
    ("Jurassic World", 4055),
    ("Charter Communications", 2467),
    ("Chick Fil A", 2244),
    ("Planet Fitness", 1868),
    ("Pitch Perfect", 1484),
    ("Express", 1112),
    ("Home", 865),
    ("Johnny Depp", 847),
    ("Lena Dunham", 582),
    ("Lewis Hamilton", 555),
    ("KXAN", 550),
    ("Mary Ellen Mark", 462),
    ("Farrah Abraham", 366),
    ("Rita Ora", 360),
    ("Serena Williams", 282),
    ("NCAA baseball tournament", 273),
    ("Point Break", 265),
]

def wordcloud_base() -> WordCloud:
    c = (
        WordCloud()
        .add("", words, word_size_range=[20, 100])
        .set_global_opts(title_opts=opts.TitleOpts(title="WordCloud-基本示例"))
    )
    return c

def wordcloud_diamond() -> WordCloud:
    c = (
        WordCloud()
        .add("", words, word_size_range=[20, 100], shape=SymbolType.DIAMOND)
        .set_global_opts(title_opts=opts.TitleOpts(title="WordCloud-shape-diamond"))
    )
    return c


#Page().add(*[fn() for fn, _ in C.charts]).render()
 
#%%打开excel，把词频统计结果放入
loadfile = load_workbook(path)
sheet = loadfile["Sheet1"]#激活sheet名为“Sheet1”的表格
sheet["C1"] = "result"
for k in range(2,len(cutlist)+2):
    sheet.cell(k,3,cutlist[k-2])
loadfile.save("/Users/apple/Desktop/ADEC7430 Big Data Econometrics/Final/file1.xlsx")

#%% rTrain
content_df = pd.read_pickle("/Users/apple/Desktop/ADEC7430 BIg Data Econometrics/Final/gzxb.pkl")

random.seed(2019)
randommask = [x < 0.5 for x in [random.uniform(0,1) for y in range(content_df.shape[0])]]

train1 = content_df.loc[randommask].copy()
train2 = content_df.loc[[not x for x in randommask]].copy()
randommask = [x < 0.7 for x in [random.uniform(0,1) for y in range(train1.shape[0])]]
train3 = train1.loc[randommask].copy()
train4 = train1.loc[[not x for x in randommask]].copy()

rTrain = train3
rValidation = train4
rTest = train2

# let's understand up a bit the data
## print out the shapes of  resultant feature data
print("\t\t\tFeature Shapes:")
print("Train set: \t\t{}".format(rTrain.shape), 
      "\nValidation set: \t{}".format(rValidation.shape),
      "\nTest set: \t\t{}".format(rTest.shape))

#%% cutlist
temp =  "\\【.*?】+|\\《.*?》+|\\#.*?#+|[.!/_,$&%^*()<>+""'?@|:~{}#]+|[——！\\\，。=？、：“”‘’￥……（）《》【】]"
texts = '\n'.join(rTrain.tolist())
#cut_text = jieba.lcut(texts)
cut_text = "".join(jieba.cut(texts))
cut_text = re.sub(pattern = temp, repl = "", string = cut_text)

keyword = jieba.analyse.extract_tags(cut_text, topK=100, allowPOS=('a','e','n','nr','ns'))  # list
cut_text = cut_text.split('\n')

#%%多个循环实现每条评论的词频统计
cutlist = []

for i in range(0, len(cut_text)):
    cut_dic = defaultdict(int) #词频不叠加，每次统计一个句子后就清空
    comment = cut_text[i]
    comment_cut = jieba.lcut(comment)
    for word in comment_cut: # word freq for every comment 
        if word in keyword:
            cut_dic[word] += 1  
    order = sorted(cut_dic.items(),key = lambda x:x[1],reverse = True) # word freq in descending order
    #print(order)
 
    myresult = {} #字典不叠加，每次统计一个句子后就清空
    for j in range(0,len(order)):#把每条评论的词频统计结果保存为str格式
        result = {order[j][0]:str(order[j][1])}
        myresult = myresult + " " + result #myresult和result的顺序不能换，否则就变升序啦 
    cutlist.append(myresult)
print(cutlist)

    cut_dic = defaultdict(int) #词频不叠加，每次统计一个句子后就清空
    comment = cut_text[6500]
    comment_cut = jieba.lcut(comment)
    for word in comment_cut: # word freq for every comment 
        if word in keyword:
            cut_dic[word] += 1  
    order = sorted(cut_dic.items(),key = lambda x:x[1],reverse = True)
    myresult = {}
    for j in range(0, len(order)):
        result = {order[j][0]: order[j][1]} 
        myresult = myresult + result 
