<div style="text-align: center; width: 100%"><img align="center" src="https://i.imgur.com/DtVMX2R.png" alt="logo"></div>

## Summary

Reddit Extractor is an R package for extracting data out of Reddit. It allows you to:

1. find subreddits based on a search query
2. find a user and their Reddit history
3. find URLs to threads of interest and retrieve comments out of these threads

## Installation

The package can be installed directly from CRAN, using `install.packages("RedditExtractoR")` or directly from GitHub via `devtools::install_github('ivan-rivera/RedditExtractor')`. Note that the latest version of this package requires R 4.1. If you have an earlier version and are not ready to upgrade, then you can install the earlier version of this package with `devtools::install_version("RedditExtractoR", version = "2.1.5", repos = "http://cran.us.r-project.org")`. Beware that version 3+ introduces significant breaking changes!

## Quick start

Let's suppose that I'd like to get the top posts from the [r/cats](https://www.reddit.com/r/cats/top) subreddit. After importing the package with `library(RedditExtractoR)` here is how we can do it:

```r
top_cats_urls <- find_thread_urls(subreddit="cats", sort_by="top")
str(top_cats_urls)
# 'data.frame':	999 obs. of  5 variables:
# $ date_utc : chr  "2021-08-15" "2021-08-13" "2021-08-11" "2021-08-07" ...
# $ title    : chr  "This went on for over 5 min" "Found this friendly stray a few months back. Now she lives with me and is doing awesome but still doesn\031t ha"| __truncated__ "Let's wake up now" "Meet the kitty sisters" ...
# $ subreddit: chr  "cats" "cats" "cats" "cats" ...
# $ comments : num  12 121 7 11 6 9 17 6 3 28 ...
# $ url      : chr  "https://www.reddit.com/r/cats/comments/p4zwkx/this_went_on_for_over_5_min/" "https://www.reddit.com/r/cats/comments/p3no3t/found_this_friendly_stray_a_few_months_back_now/" "https://www.reddit.com/r/cats/comments/p21kyf/lets_wake_up_now/" "https://www.reddit.com/r/cats/comments/oztuiq/meet_the_kitty_sisters/" ...
```

Note that we simply got the top threads from a subreddit of interest. We could also use keywords to look for threads of interest, e.g.: `find_thread_urls(keywords="cute kittens")`.

In some situations this could well be all you are after, but in most cases you'll probably want to parse these URLs and retrieve their metadata and comments. Here we go:

```r
threads_contents <- get_thread_content(top_cats_urls$url[1:2]) # for the sake of simplicity
str(threads_contents$threads) # thread metadata
# 'data.frame':	2 obs. of  14 variables:
# $ url                  : chr  "https://www.reddit.com/r/cats/comments/p4zwkx/this_went_on_for_over_5_min/" "https://www.reddit.com/r/cats/comments/p3no3t/found_this_friendly_stray_a_few_months_back_now/"
# $ author               : chr  "CasterQ" "Just2063"
# $ date                 : chr  "2021-08-15" "2021-08-13"
# $ title                : chr  "This went on for over 5 min" "Found this friendly stray a few months back. Now she lives with me and is doing awesome but still doesn\031t ha"| __truncated__
# $ text                 : chr  "" ""
# $ subreddit            : chr  "cats" "cats"
# $ score                : num  320 322
# $ upvotes              : num  320 322
# $ downvotes            : num  0 0
# $ up_ratio             : num  1 0.99
# $ total_awards_received: num  2 1
# $ golds                : num  0 0
# $ cross_posts          : num  0 0
# $ comments             : num  12 121
str(threads_contents$comments)
# 'data.frame':	132 obs. of  9 variables:
# $ url       : chr  "https://www.reddit.com/r/cats/comments/p4zwkx/this_went_on_for_over_5_min/" "https://www.reddit.com/r/cats/comments/p4zwkx/this_went_on_for_over_5_min/" "https://www.reddit.com/r/cats/comments/p4zwkx/this_went_on_for_over_5_min/" "https://www.reddit.com/r/cats/comments/p4zwkx/this_went_on_for_over_5_min/" ...
# $ author    : chr  "stinkadinkalink" "CasterQ" "Future_Branch_8629" "dancingwithpenguins" ...
# $ date      : chr  "2021-08-15" "2021-08-15" "2021-08-15" "2021-08-15" ...
# $ score     : num  21 10 6 3 5 3 13 2 2 2 ...
# $ upvotes   : num  21 10 6 3 5 3 13 2 2 2 ...
# $ downvotes : num  0 0 0 0 0 0 0 0 0 0 ...
# $ golds     : num  0 0 0 0 0 0 0 0 0 0 ...
# $ comment   : chr  "such a cute cat but omg how many monitors do they have!!!! that look like nasa type set up" "Haha my husband is a computer software engineer the extra monitors are for his work" "Totally came here to say I have monitor jealousy. Glad he can take a break with his buddy." "=\002 this is so cute!=;" ...
# $ comment_id: chr  "1" "1_1" "1_1_1" "2" ...
```

If you'd like to join comments and their parent threads, you can do this by the URL.

Sometimes you might actually be looking for subreddits rather than threads, if so, we've got you covered too. Let's assume that we are trying to find subreddits about cats:

```r
cat_subreddits <- find_subreddits("cats")
# 'data.frame':	248 obs. of  6 variables:
# $ id         : chr  "3gl3k" "2vi0z" "30tmh" "2tteh" ...
# $ date_utc   : chr  "2016-09-28" "2012-11-08" "2014-03-05" "2012-03-29" ...
# $ subreddit  : chr  "MemeEconomy" "Awwducational" "TwoSentenceHorror" "Justrolledintotheshop" ...
# $ title      : chr  "MemeEconomy" "Awwducational" "Two-Sentence Horror Stories: Bite-sized scares. " "Just Rolled Into the Shop" ...
# $ description: chr  "/r/MemeEconomy is a place where individuals can buy, sell, share, make, and invest in templates freely.\n\n\nv2"| __truncated__ "Don't just waste time, learn something too!" "Give us your scariest story in two sentences (or less)!" "For those absolutely stupid things that you see people bring, roll, or toss into your place of business and the"| __truncated__ ...
# $ subscribers: num  1430743 2965466 678423 1365589 586672 ..
```

Now you could technically feed these subreddits in a loop into the thread finder to generate a massive dataset.

Lastly, let's suppose that you'd like to retrieve information about a particular user:

```r
nat_geo_user <- get_user_content("nationalgeographic")
str(nat_geo_user$about)
# List of 7
# $ created_utc  : chr "2017-08-24"
# $ name         : chr "nationalgeographic"
# $ is_employee  : logi FALSE
# $ is_mod       : logi TRUE
# $ is_gold      : logi TRUE
# $ thread_karma : num 279068
# $ comment_karma: num 87406
str(nat_geo_user$comments)
# 'data.frame':	997 obs. of  11 variables:
# $ url           : chr  "https://www.reddit.com/r/history/comments/anhdnl/im_historian_author_and_musician_mark_lee_gardner/" "https://www.reddit.com/r/history/comments/anhdnl/im_historian_author_and_musician_mark_lee_gardner/" "https://www.reddit.com/r/history/comments/anhdnl/im_historian_author_and_musician_mark_lee_gardner/" "https://www.reddit.com/r/history/comments/anhdnl/im_historian_author_and_musician_mark_lee_gardner/" ...
# $ date_utc      : chr  "2019-02-05" "2019-02-05" "2019-02-05" "2019-02-05" ...
# $ subreddit     : chr  "history" "history" "history" "history" ...
# $ thread_author : chr  "nationalgeographic" "nationalgeographic" "nationalgeographic" "nationalgeographic" ...
# $ comment_author: chr  "nationalgeographic" "nationalgeographic" "nationalgeographic" "nationalgeographic" ...
# $ thread_title  : chr  "I\031m historian, author, and musician Mark Lee Gardner and I can tell you a lot about Jesse James, the infamou"| __truncated__ "I\031m historian, author, and musician Mark Lee Gardner and I can tell you a lot about Jesse James, the infamou"| __truncated__ "I\031m historian, author, and musician Mark Lee Gardner and I can tell you a lot about Jesse James, the infamou"| __truncated__ "I\031m historian, author, and musician Mark Lee Gardner and I can tell you a lot about Jesse James, the infamou"| __truncated__ ...
# $ comment       : chr  "You're welcome.  Part of Jesse and Frank's success in eluding law enforcement (in addition to fast horses!) was"| __truncated__ "In 1874, the Missouri legislature passed the Suppression of Outlawry Act that set aside a \"state secret servic"| __truncated__ "Probably the quick-draw gunfight: two men staring each other down in the middle of the street and attempting to"| __truncated__ "That was actually part of the problem.  There was often little coordination between towns and the state.  In fa"| __truncated__ ...
# $ score         : num  2 2 9 4 8 6 6 10 13 2 ...
# $ up            : num  2 2 9 4 8 6 6 10 13 2 ...
# $ downs         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ golds         : num  0 0 0 0 0 0 0 0 0 0 ...
str(nat_geo_user$threads)
# 'data.frame':	999 obs. of  10 variables:
# $ url      : chr  "https://www.nationalgeographic.com/environment/2019/02/2018-fourth-warmest-year-ever-noaa-nasa-reports/?cmpid=o"| __truncated__ "https://www.reddit.com/r/history/comments/anhdnl/im_historian_author_and_musician_mark_lee_gardner/" "https://www.nationalgeographic.com/environment/2019/02/climate-change-alters-oceans-blues-greens/" "https://v.redd.it/ehqa55sbcge21" ...
# $ date_utc : chr  "2019-02-06" "2019-02-05" "2019-02-05" "2019-02-04" ...
# $ subreddit: chr  "u_nationalgeographic" "history" "u_nationalgeographic" "u_nationalgeographic" ...
# $ author   : chr  "nationalgeographic" "nationalgeographic" "nationalgeographic" "nationalgeographic" ...
# $ title    : chr  "The last five years were the hottest ever, NASA and NOAA declare" "I\031m historian, author, and musician Mark Lee Gardner and I can tell you a lot about Jesse James, the infamou"| __truncated__ "Climate change will shift the oceansâ\u0080\u0099 colors" "Welcome to our half-time show of only the most superb owls" ...
# $ text     : chr  "" "Edit: Thanks so much for your questions.  They were excellent!  I've got to run now, but be sure to check out m"| __truncated__ "" "" ...
# $ golds    : num  0 0 0 0 2 0 1 0 0 0 ...
# $ score    : num  303 62 165 97 6406 ...
# $ ups      : num  303 62 165 97 6406 ...
# $ downs    : num  0 0 0 0 0 0 0 0 0 0 ...
```

That's all there is to it!

## Contributing

If you'd like to improve this package, there are several ways that you can help. First, if you spot a bug or if you'd like to propose a new feature, then please create an issue. If you'd like to implement a bugfix or a new feature, then please create a pull request. 

## FAQ

---

* **Question**: Why can I not get all the comments out of a thread?
* **Answer**: The [Reddit API](https://www.reddit.com/dev/api/) limits how much data you can retrieve and there is no way around it right now. If you'd like to get a larger sample of data, consider trying data dumps like [PushShift](https://files.pushshift.io/reddit/comments/). 

---

* **Question**: All functions in this library appear to be a little slow, why is that?
* **Answer**: The Reddit API allows user to make 60 requests per minute (1 request per second), which is why URL parsers used in this library intentionally limit requests to conform to the API requirements

---

* **Question**: `find_thread_urls` and `find_subreddits` functions do not always include keyword terms used in the search, is that a bug?
* **Answer**: No it is not a bug. We simply pass your search query to the Reddit API and it returns whatever results it can find semantically based not only on your keyword inputs but also your `sort_by` choice

---

* **Question**: I'd like to add more functionality into the package, can you help?
* **Answer**: You might like to check the [Reddit API](https://www.reddit.com/dev/api/) to see if your idea is feasible and if it is, then either create an issue or a pull request.

---
