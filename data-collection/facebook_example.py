import yaml
import urllib
from time import sleep
import datetime
import json
import os

# Script to take a list of parties (already specified via CrowdTangle UI) and 
# download data to JSON file using the CrowdTangle API
# CrowdTangle Access for this project was provided by Social Science One
# Meta plans to shutdown the CrowdTangle service in August 2024 so these scripts 
# will be unlikely to work in the future


# Read credentials for CrowdTangle
with open("facebook_creds.yml", 'r') as ymlfile:
    creds = yaml.safe_load(ymlfile)


# Functions
def saveData(post):
    """
    Takes a post object, extracts relevant information and dumps JSON
    Note: Can also be modified to insert into a postgres database
    """

    def getHandle(post):
        """Not all have handle. If not, just leave as empty string"""
        try:
            handle = post['account']['handle']
        except:
            handle = ""
        return handle

    info = {"page_id" : post['account']['id'],
            "handle": getHandle(post),
            "name": post['account']['name'],
            "post_id": int("".join(filter(str.isdigit, post['id']))), # edited this line due to changes in the formatting of post IDs
            "date_added": post['date'], # Note that this will need to be converted to datatime for extraction
            "type": post['type'],
            "link": post['postUrl'],
            "likes_actual": post['statistics']['actual']['likeCount'],
            "shares_actual": post['statistics']['actual']['shareCount'],
            "comments_actual": post['statistics']['actual']['commentCount'],
            "love_actual": post['statistics']['actual']['loveCount'],
            "wow_actual": post['statistics']['actual']['wowCount'],
            "haha_actual": post['statistics']['actual']['hahaCount'],
            "sad_actual": post['statistics']['actual']['sadCount'],
            "angry_actual": post['statistics']['actual']['angryCount'],
            "likes_expected": post['statistics']['expected']['likeCount'],
            "shares_expected": post['statistics']['expected']['shareCount'],
            "comments_expected":post['statistics']['expected']['commentCount'],
            "love_expected": post['statistics']['expected']['loveCount'],
            "wow_expected": post['statistics']['expected']['wowCount'],
            "haha_expected": post['statistics']['expected']['hahaCount'],
            "sad_expected": post['statistics']['expected']['sadCount'],
            "angry_expected":post['statistics']['expected']['angryCount'],
            "score": round(post['score'], 3),
            "data": json.dumps(post)}

    with open("../data/facebook_data_raw/facebook_data.json", "a") as f:
                json.dump(info, f)
                f.write(os.linesep)

def retrieveData(query):
    """This function takes an API query (e.g. all posts between t and t+1) and continues
    to make API calls until the pagination is completed.

    It accounts for the rate limits by sleeping between calls

    Returns a tuple containing the earliest date (date of the final post) and the number of posts retrieved
    """
    # Make initial query
    print(query)
    response = urllib.request.urlopen(query)
    data = json.loads(response.read())

    # If query, is valid, store information in a list
    if data['status'] == 200:
        for x in data['result']['posts']:
            saveData(x)
        return datetime.datetime.strptime(data['result']['posts'][-1]['date'],'%Y-%m-%d %H:%M:%S'), len(data['result']['posts'])
    else: # otherwise print an error code
        print("Error, status code ", str(data['status']))

def queryConstructor(end, token, listid):
    """This function takes an end datetime, an API token, and a list ID.

    It returns a string corresponding to the relevant API call. This can then be passed to paginate."""
    string_components = []
    string_components.append("https://api.crowdtangle.com/posts?token=")
    string_components.append(token)
    string_components.append("&sortBy=date&endDate=")
    string_components.append(end.strftime("%Y-%m-%dT%H:%M:%S"))
    string_components.append("&listIds=")
    string_components.append(str(listid))
    string_components.append("&count=1000") # only getting 1k at a time to avoid timeout errors
    return "".join(string_components)


if __name__ == '__main__':

    # Load CrowdTangle token and list ID
    token = creds['crowdtangle']['access_token']
    listid = 99999 # listid corresponds to unique list number from crowdtangle (this is a placeholder)


    d = datetime.datetime.now() # Initial start time is now
    month = 1 # Setting initial month
    N = 0 # number of posts processed.
    while d > datetime.datetime(2010,1,1,0,0,0): # Count down until we get to this date.
        query = queryConstructor(d, token, listid) # construct the query
        d, n = retrieveData(query) # Retrieve, process, and store data. New d is the earliest date returned.
        N += n # increment post processed count
        print(d, N) # print new end date and number processed.
        if d.month != month:
            month = d.month
