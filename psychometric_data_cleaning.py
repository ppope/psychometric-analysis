

import sys
import os
import json
import pandas as pd
import collections

#Specify data directory below.
mainDir = '/home/ubuntu/'
dataDir = mainDir + 'DeepMile/assignment/OCEAN_Social_Data'
os.chdir(dataDir)

#Useful debugging functions, From "Python for Data Analysis" P.65
def set_trace():
    from IPython.core.debugger import Pdb
    Pdb(color_scheme='Linux').set_trace(sys._getframe().f_back)
    
def debug(f, *args, **kwargs):
    from IPython.core.debugger import Pdb
    pdb = Pdb(color_scheme='Linux')
    return pdb.runcall(f,*args,**kwargs)


#http://stackoverflow.com/questions/6027558/flatten-nested-python-dictionaries-compressing-keys
def flatten(d, parent_key='', sep='_'):
    items = []
    for k, v in d.items():
        new_key = parent_key + sep + k if parent_key else k
        if isinstance(v, collections.MutableMapping):
            items.extend(flatten(v, new_key).items())
        else:
            items.append((new_key, v))
    return dict(items)


def collapse_raw_features(group):
    gender = group['demographic_gender'][group.index[0]]
    klout_score = group['klout_score'].max()
    followers_count = group['twitter_user_followers_count'].max()
    favorites_count = group['twitter_user_favourites_count'].max()
    return pd.Series({'gender' : gender, 
                      'klout_score' : klout_score, 
                      'followers_count' : followers_count, 
                      'favorites_count' : favorites_count})

def concatenate_tweets(group):
    tweet_doc = ', '
    tweet_doc = tweet_doc.join(group['interaction_content'])
    return tweet_doc

def get_profiles(group):
    return group['twitter_user_description'][group.index[0]]

def sentiment_count(text):
    text = text.lower()
    words = text.split(' ')
    positive_count, negative_count = 0,0
    for word in words:
       if word in positive_words:
            positive_count += 1
       if word in negative_words:
            negative_count += 1
    return pd.Series({'positive_count' : positive_count, 'negative_count' : negative_count})

def get_sum_hashtags_and_mentions(group):
    hashtag_count = group['len_interaction_hashtags'].sum()
    user_mention_count = group['len_interaction_mentions'].sum()
    return pd.Series({'hashtag_count' : hashtag_count, 'user_mention_count' : user_mention_count})
#Load twitter data


file_list = os.listdir(dataDir)
#file_list = ['15JUN-9ca1cf868c49351368a675cbf92af582-1403380695.json', '15JUN-46dfb2bed80ca5196591706d69715f33-1403309844.json']

temp_list = []
for file_path in file_list:   
    with open (file_path, "r") as myfile:
        temp_data = myfile.readlines()
    for line in temp_data:
        try:
            temp = json.loads(line)
        except ValueError:
            next
        temp_list.append(flatten(temp))


data = pd.DataFrame(temp_list)

#Load positive and negative word lists to be used for sentiment analysis of twitter posts and user profile.

positive_words_path = mainDir + '/DeepMile/assignment/positive-words.txt'
negative_words_path = mainDir + '/DeepMile/assignment/negative-words.txt'

with open(positive_words_path) as f:
    lines1 = f.readlines()

with open(negative_words_path) as f:
    lines2 = f.readlines()
    
positive_words = [x.strip('\n') for x in lines1]
negative_words = [x.strip('\n') for x in lines2]


#Two fields in the data are feasible for text processing:  
#user tweets (interaction_content) and user profile (twitter_user_description).
#We summarize the sentiment of these fields by counting the number of positive or negative words.
#We implement this from predetermined list of positive and negative words (thanks John for the lists).

#There is some processing to the data that must be done first.
#Each user may have multiple tweets. First we must concatenate the tweets into a single 'document'.
tweets = data[['interaction_author_username', 'interaction_content']].groupby('interaction_author_username').apply(concatenate_tweets)

#We assume the user profile is a static among different tweets by a user. 
#This assumption is reasonable if the user doesn't edit their profile too frequently.
#We extract the first occurence of a profile from the data.
#Some profiles are left blank. We denote blank profiles by a '' value.
profiles = data[['interaction_author_username', 'twitter_user_description']].groupby('interaction_author_username').apply(get_profiles)
profiles = profiles.fillna('')

#Calculate the positive and negative sentiments of the profiles and tweets.
tweet_sentiments = tweets.apply(sentiment_count)
profile_sentiments = profiles.apply(sentiment_count)
#Rename columns for organization.
tweet_sentiments.columns = ['tweet_negative_word_count', 'tweet_positive_word_count']
profile_sentiments.columns = ['profile_negative_word_count', 'profile_positive_word_count']


#Note following warning arises due to the presence of unicode characters in the tweets:
#-c:39: UnicodeWarning: Unicode equal comparison failed to convert both arguments to Unicode - interpreting them as being unequal


#Compute number of hashtags mentioned and number of other users mentioned for each user.
#The number of other users mentioned in a tweets gives an indication of how social the tweeting user is.
#Likewise, for the number of hashtags mentioned. 
mentions_and_hashtags = data[['interaction_author_username', 'interaction_hashtags', 'interaction_mentions']].fillna('')
mentions_and_hashtags['len_interaction_hashtags'] = mentions_and_hashtags['interaction_hashtags'].apply(len)
mentions_and_hashtags['len_interaction_mentions'] = mentions_and_hashtags['interaction_mentions'].apply(len)

temp = mentions_and_hashtags[['interaction_author_username', 'len_interaction_hashtags', 'len_interaction_mentions']]
count_mentions_and_hashtags = temp.groupby('interaction_author_username').apply(get_sum_hashtags_and_mentions)


#Compute character length of user description, and interaction content.
profile_lengths = pd.DataFrame(profiles.apply(len), columns=["nchar_profile"])
tweet_lengths = pd.DataFrame(tweets.apply(len), columns=["nchar_tweets"])


#Collapse data across different tweets by the same user.
#These variables ought to be the same across different tweets for a user.
raw_features = data[['interaction_author_username','demographic_gender', 
                   'twitter_user_favourites_count', 'twitter_user_followers_count', 'twitter_user_friends_count', 'klout_score']]
collapsed_raw_features = raw_features.groupby('interaction_author_username').apply(collapse_raw_features)
collapsed_raw_features['gender'] = collapsed_raw_features['gender'].fillna(0)

#Fill in missing Klout Scores with 0
collapsed_raw_features['klout_score'] = collapsed_raw_features['klout_score'].fillna(0)

#Merge predictor variables. Then merge all variables into a regression table.
predictors = pd.concat([tweet_sentiments, profile_sentiments, count_mentions_and_hashtags, collapsed_raw_features, profile_lengths, tweet_lengths], axis=1)
response = pd.read_csv(mainDir + 'DeepMile/assignment/OCEAN Summary Survey Results by Twitter UserName.csv', index_col='user_screen_name')
regression_table = pd.merge(response, predictors, left_index=True, right_index=True)

regression_table.to_csv(mainDir + '/DeepMile/regression_table.csv', index=False)

