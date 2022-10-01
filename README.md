# job-stats

I wanted to see if the Elm slack #jobs channel was getting more/less job posts over time. This is my results:
![image](https://user-images.githubusercontent.com/5068391/193423555-22136f19-f5d9-49be-8e54-f9a5c8cc71a2.png)

## Reproducing this data

The way I collected the data was by:
1. Go to #jobs channel in Elm slack
2. Open dev tools in Firefox and go to the network tab
3. Refresh the page
4. Start scrolling up until you reach the beginning of the channel (make sure you do this during one of the free trial periods Slack sometimes gives, otherwise you won't see any messages older than a month)
5. Once you've reached the beginning, export all the network traffic to a .har file
![image](https://user-images.githubusercontent.com/5068391/193423731-88a6076b-f381-4adb-b9ec-817953fbddde.png)
6. Clone this repo and add a folder called public.
7. Add a file in it called `network-data.har`
8. Download Lamdera https://dashboard.lamdera.app/docs/download and then run `lamdera live` in your terminal
9. You should see the graph at the top of the screen and all the messages below!
