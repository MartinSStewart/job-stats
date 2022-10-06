# job-stats

I wanted to see if the Elm slack #jobs channel was getting more/less job posts over time. This is my results:
![image](https://user-images.githubusercontent.com/5068391/193423555-22136f19-f5d9-49be-8e54-f9a5c8cc71a2.png)

## About the data

[Janiczek](https://github.com/Janiczek) exported the #jobs channel messages for me (thanks!). I then by hand went through all the messages picking out job postings about Elm that looked serious. This took about 30 minutes to do so it's possible I made mistakes. Also some companies post the same opening again after a few months. I decided to count these mostly because it was easier to do. If you don't think they should be included then consider this graph a slight overestimation.

## Reproducing this graph

1. Download Lamdera https://dashboard.lamdera.app/docs/download and then run `lamdera live` in your terminal while in the root folder for this repo
2. Go to localhost:8000
3. You should see the graph at the top of the screen and all the messages below!
