# RedditExtractor

## Version 3.0.7

`get_thread_content` can now handle errors without interrupting parsing

## Version 3.0.6

Added timestamps to extracted comments and threads

## Version 3.0.5

Fixed a bug in comment extraction which occurred when a thread would report more than zero comments even none were present

## Version 3.0.4

* Added self-text into the data frame with thread URLs

## Version 3.0.3

* Updated the `get_user_content` method so that it can now accept a vector of users

## Version 3.0.2

* Fixed the URL encoding problem that affected URLs with special characters, see [this issue](https://github.com/ivan-rivera/RedditExtractor/issues/17) for more information

## Version 3.0.0

* Overhauled the package making it more readable/maintainable
* Changed the interface for all functions
* Added new functionality to look up users and subreddits
* Divided thread extraction into thread metadata and comments
* Removed unnecessary dependencies
* Fixed a bug concerning special characters in the URLs
* Added an FAQ into the README
