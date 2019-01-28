
# coding: utf-8

# # Sentiment Analysis on Twitter with Python

# Financial Programming Individual Assignment

# By: Paulina Valencia

# ## Sentiment Analysis 

# Sentiment analysis is the process of computationally identifying and categorizing opinions expressed in a piece of text, in order to determine whether the text's attitude towards a particular topic, product, etc. is positive, negative, or neutral.
# 
# The applications for sentiment analysis are endless. More and more we’re seeing it used in social media monitoring and VOC to track customer reviews, survey responses, competitors, etc. However, it is also practical for use in business analytics and situations in which text needs to be analyzed. 
#  
# Sentiment analysis is in demand because of its efficiency. Thousands of text documents can be processed for sentiment (and other features including named entities, topics, themes, etc.) in seconds, compared to the hours it would take a team of people to manually complete. Because it is so efficient (and accurate – Semantria  has 80% accuracy for English content) many businesses are adopting text and sentiment analysis and incorporating it into their processes.

# In this program we will perform a sentiment analysis using data from Twitter. This will be done in python, so first we need to install the following packages: 
# * Tweepy
# * Tkinter
# * TextBlob

# ## Tweepy

# Tweepy is open-sourced, hosted on GitHub and enables Python to communicate with Twitter platform and use its API.
# 
# Tweepy supports accessing Twitter via Basic Authentication and the newer method, OAuth. Twitter has stopped accepting Basic Authentication so OAuth is now the only way to use the Twitter API.

# ## Tkinter

# Tkinter is Python's de-facto standard GUI (Graphical User Interface) package. 

# ## TextBlob

# TextBlob is a Python library for processing textual data. It provides a simple API for diving into common natural language processing (NLP) tasks.
# It is easy to use and offers a lot of features like **sentiment analysis**, pos-tagging, noun phrase extraction, tokenization, etc. 
# 
# The sentiment property in TextBlob returns a namedtuple of the form Sentiment(polarity, subjectivity). The polarity score is a float within the range [-1.0, 1.0]. The subjectivity is a float within the range [0.0, 1.0] where 0.0 is very objective and 1.0 is very subjective.

# In order to install this 3 packages we need to run the following lines in Anaconda Prompt:

# * pip install tweepy
# * pip install tkinter
# * pip install textblob

# Then we need to import the following packages into Python:

# In[1]:


import tweepy
from tweepy import OAuthHandler
import pandas as pd
import numpy as np
from IPython.display import display
from tkinter import *
from time import sleep
from datetime import datetime
from textblob import TextBlob
import matplotlib.pyplot as plt


# Now we need to link a Twitter account to our script. A Twitter account is needed so if you don’t have one already, create one. Then, your account needs to be changed into "developper" because an App will be created. To do this, go to apps.twitter.com and sign in with your account. Create a Twitter application and generate a Consumer Key, Consumer Secret, Access Token, and Access Token Secret.

# In[2]:


consumer_key="vXkdx5hnNdE293ahvs5wxWARs"
consumer_secret="sBKCzVHnicSd0XYE4lGREqmrLdbEw6mwXgo5uHFedAoueRMYRb"
access_token="569483185-1WWyz4AvGxmO3anIsaemv0I1K7DMNeNG4lqQIQGD"
access_secret="LVCUnKJAx1gberzXGLFiGEXYvXppz7krLlWHuzv2ehGb4"


# In[14]:


auth = OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_secret)
api = tweepy.API(auth)


# Once this is done, we can create a function that extracts tweets.

# ## Part 1 : Tweet analysis on a Twitter account

# In[3]:


def twitter_setup():
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_secret)
    api = tweepy.API(auth)
    return api


# To check if your connection works, you can try extracting some tweets. You can write the name of any account and check the tweets it has made. 

# In[3]:


account = "choose a Twitter account"
count = "choose number of tweets"


# In[5]:


extractor = twitter_setup()
tweets = extractor.user_timeline(screen_name=account, count=count)
print("Number of tweets: {}.\n".format(len(tweets)))
print("10 latest tweets:\n")
for tweet in tweets[:10]:
    print(tweet.text)
    print()


# Then we are going to store this tweets information in a data frame.

# In[6]:


data=pd.DataFrame(data=[tweet.text for tweet in tweets], columns=['Tweets'])
data['length']=np.array([len(tweet.text) for tweet in tweets])
data['ID']=np.array([tweet.id for tweet in tweets])
data['date']=np.array([tweet.created_at for tweet in tweets])
data['source']=np.array([tweet.source for tweet in tweets])
data['likes']=np.array([tweet.favorite_count for tweet in tweets])
data['RTs']=np.array([tweet.retweet_count for tweet in tweets])

display(data.head(10))    


# Now that we have a dataframe with tweets from the chosen account, we can start calculating some insights:

# In[7]:


# Average lenght of tweets:
mean = np.mean(data['length'])
print("The average lenght of tweets is: {}".format(mean))

# Tweet with most likes and most retweets:
fav_max = np.max(data['likes'])
rt_max  = np.max(data['RTs'])
fav = data[data.likes == fav_max].index[0]
rt  = data[data.RTs == rt_max].index[0]

# Likes:
print("The tweet with most likes is: \n{}".format(data['Tweets'][fav]))
print("Number of likes: {}".format(fav_max))
print("{} caracters.\n".format(data['length'][fav]))

# Retweets:
print("The most retweeted tweet is: \n{}".format(data['Tweets'][rt]))
print("Number of RTs: {}".format(rt_max))
print("{} caracters.\n".format(data['length'][rt]))


# I may be interesting to find if there is a correlation between the number of likes and the number of retweets in a tweet.

# Now we want to create time series with this tweet information:

# In[8]:


tlen = pd.Series(data=data['length'].values, index=data['date'])
tfav = pd.Series(data=data['likes'].values, index=data['date'])
tret = pd.Series(data=data['RTs'].values, index=data['date'])


# Now we plot this data:

# In[10]:


# Lenght variation of tweets with time:
tlen.plot(figsize=(16,4), color='r')


# In[12]:


# Like and retweet variation with time:
tfav.plot(figsize=(16,4), label="Like", legend=True)
tret.plot(figsize=(16,4), label="RTs", legend=True);


# ## Part 2 : Sentiment analysis on a keyword in Twitter 

# Now we will create a graphical user interface (GUI) so that a user can choose a keyword in Twitter and perform the sentiment analysis about it. For this, we will use two labels: one for the search and the other for the sample size or number of tweets to be analyzed. We will also need a submit button so that when clicked, we can call our getData function.

# In[15]:


#GUI
root = Tk()
label1 = Label(root, text="Search")
E1 = Entry(root, bd =5)
label2 = Label(root, text="Sample Size")
E2 = Entry(root, bd =5)

def getE1():
    return E1.get()

def getE2():
    return E2.get()


# In the next step we will define a function that gets information about the tweets based on the keyword chosen and then analyzes it using TextBlob functions like "sentiment" and "polarity". Then the function plots the values obtained from the sentiment analysis. Values >0 are positive sentiments and values <0 are negative sentiments.

# In[16]:


def getData():
    getE1()
    keyword = getE1()

    getE2()
    numberOfTweets = getE2()
    numberOfTweets = int(numberOfTweets)

    #Where the tweets are stored to be plotted
    polarity_list = []
    numbers_list = []
    number = 1

    for tweet in tweepy.Cursor(api.search, keyword, lang="en").items(numberOfTweets):
        try:
            analysis = TextBlob(tweet.text)
            analysis = analysis.sentiment
            polarity = analysis.polarity
            polarity_list.append(polarity)
            numbers_list.append(number)
            number = number + 1

        except tweepy.TweepError as e:
            print(e.reason)

        except StopIteration:
            break

    #Plotting
    axes = plt.gca()
    axes.set_ylim([-1, 2])

    plt.scatter(numbers_list, polarity_list)

    averagePolarity = (sum(polarity_list))/(len(polarity_list))
    averagePolarity = "{0:.0f}%".format(averagePolarity * 100)
    time  = datetime.now().strftime("At: %H:%M\nOn: %m-%d-%y")

    plt.text(0, 1.25, "Average Sentiment:  " + str(averagePolarity) + "\n" + time, fontsize=12, bbox = dict(facecolor='none', edgecolor='black', boxstyle='square, pad = 1'))

    plt.title("Sentiment of " + keyword + " on Twitter")
    plt.xlabel("Number of Tweets")
    plt.ylabel("Sentiment")
    plt.show()


# In[17]:


submit = Button(root, text ="Submit", command = getData)

label1.pack()
E1.pack()
label2.pack()
E2.pack()
submit.pack(side =BOTTOM)

root.mainloop()


# By using the GUI we can change the keywords in order to find useful insights about any topic!

# ## In conclusion 

# There is a wealth of information out there hidden in individuals comments, emails, tweets, form submissions, reviews and the challenge is wrangling all of this info and extracting value from it because of the lack of structure in these types of data.
# 
# Sentiment analysis is extremely useful in social media monitoring as it allows us to gain an overview of the wider public opinion behind certain topics. However, challenges still exist as sentiment analysis can't still recognize complicated data such as sarcasm, irony, multiple languages, images, and video posts. Computer technology has not yet caught up to interpret these complicated data within the right context.
# 
# For the future, to truly understand and capture the broad range of emotions that humans express as written word, we need a more sophisticated multidimensional scale.
# 
# Organisations will certainly become more aware of the applications of sentiment analysis within their marketplace, fuelling the growth of sector specific services and technology delivering sentiment specific use cases.
# 
# 

# # Thank you
