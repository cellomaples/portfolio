
import requests
from bs4 import BeautifulSoup
import json
from datetime import datetime
import pandas as pd
import time


# Set Headers
headers = {
    'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36 OPR/68.0.3618.125'
}

# Username and Password must be entered here
login_data = {
    'txtUserName': '',
    'txtPassword': '',
    'btnLogin': 'Login'
}

# Dataframe for storing counts. No purpose other than recording data.
ordercounts = pd.DataFrame(columns=['time','location1_cart','location1_confirmed','location2_cart','location2_confirmed',
                                    'location3_cart','location3_confirmed','location4_cart','location4_confirmed',
                                    'location5_cart','location5_confirmed','location6_cart','location6_confirmed'])

# Initiate request session
with requests.Session() as s:
    # Login URL
    url = "https://website.lfmadmin.com/legacy/admin/Login.aspx"
    # Locations URL for get requests
    locUrl = 'https://website.lfmadmin.com/grow/api/Setup/?setting=locations'
    # URL for modifying locations with put requests
    stopUrl = 'https://website.lfmadmin.com/grow/api/Setup/?setting='
    
    
    r = s.get(url, headers = headers)
    soup = BeautifulSoup(r.content, 'html5lib')
    login_data['__VIEWSTATE'] = soup.find('input', attrs = {'name': '__VIEWSTATE'})['value']
    login_data['__VIEWSTATEGENERATOR'] = soup.find('input', attrs = {'name': '__VIEWSTATEGENERATOR'})['value']
    login_data['__EVENTVALIDATION'] = soup.find('input', attrs = {'name': '__EVENTVALIDATION'})['value']
    
    # Login to site
    s.post(url, data = login_data, headers = headers)
    
    # URL pages for orders: Modify periodId= to current period, modify customerLocation= for locations, orderStatus=COMPLETE are confirmed orders
    url1 = 'https://website.lfmadmin.com/grow/api/OrderSearch/?exportCSVFile=false&searchType=advanced&firstLoad=false&orderId=&periodId=52&subperiodId=-1&keyword=&customerLocation=5&routeId=-1&orderStart=&orderEnd=&distStart=&distEnd=&orderMethod=-1&orderStatus=COMPLETE&payStatus=-1&locationType=-1&orderType=-1&sortOrder=date&orderTotal=any&minTotal=&page=1'
    url2 = 'https://website.lfmadmin.com/grow/api/OrderSearch/?exportCSVFile=false&searchType=advanced&firstLoad=false&orderId=&periodId=52&subperiodId=-1&keyword=&customerLocation=6&routeId=-1&orderStart=&orderEnd=&distStart=&distEnd=&orderMethod=-1&orderStatus=COMPLETE&payStatus=-1&locationType=-1&orderType=-1&sortOrder=date&orderTotal=any&minTotal=&page=1'
    url3 = 'https://website.lfmadmin.com/grow/api/OrderSearch/?exportCSVFile=false&searchType=advanced&firstLoad=false&orderId=&periodId=52&subperiodId=-1&keyword=&customerLocation=7&routeId=-1&orderStart=&orderEnd=&distStart=&distEnd=&orderMethod=-1&orderStatus=COMPLETE&payStatus=-1&locationType=-1&orderType=-1&sortOrder=date&orderTotal=any&minTotal=&page=1'
    url4 = 'https://website.lfmadmin.com/grow/api/OrderSearch/?exportCSVFile=false&searchType=advanced&firstLoad=false&orderId=&periodId=52&subperiodId=-1&keyword=&customerLocation=8&routeId=-1&orderStart=&orderEnd=&distStart=&distEnd=&orderMethod=-1&orderStatus=COMPLETE&payStatus=-1&locationType=-1&orderType=-1&sortOrder=date&orderTotal=any&minTotal=&page=1'
    url5 = 'https://website.lfmadmin.com/grow/api/OrderSearch/?exportCSVFile=false&searchType=advanced&firstLoad=false&orderId=&periodId=52&subperiodId=-1&keyword=&customerLocation=9&routeId=-1&orderStart=&orderEnd=&distStart=&distEnd=&orderMethod=-1&orderStatus=COMPLETE&payStatus=-1&locationType=-1&orderType=-1&sortOrder=date&orderTotal=any&minTotal=&page=1'
    url6 = 'https://website.lfmadmin.com/grow/api/OrderSearch/?exportCSVFile=false&searchType=advanced&firstLoad=false&orderId=&periodId=52&subperiodId=-1&keyword=&customerLocation=10&routeId=-1&orderStart=&orderEnd=&distStart=&distEnd=&orderMethod=-1&orderStatus=COMPLETE&payStatus=-1&locationType=-1&orderType=-1&sortOrder=date&orderTotal=any&minTotal=&page=1'
    
    
    ## Loop to monitor orders ##
    
    # Set triggers = False, will change to True when location is hidden/closed
    trigger1 = False
    trigger2 = False
    trigger3 = False
    trigger4 = False
    trigger5 = False
    trigger6 = False
    
    # Constant for while loop, will change to i = 0 when all triggers = False
    i = 1
    
    while i == 1:
    
        # Scrape order pages contents
        pageContent1 = s.get(url1)
        pageContent2 = s.get(url2)
        pageContent3 = s.get(url3)
        pageContent4 = s.get(url4)
        pageContent5 = s.get(url5)
        pageContent6 = s.get(url6)
        
        # Decode from bytes to string
        mystr1 = pageContent1.content.decode("utf-8")
        # Load string as json dictionary
        my_dict1 = json.loads(mystr1)
        # Extract completed order count value from key
        ordersComplete1 = int(my_dict1['orderCount'])

        mystr2 = pageContent2.content.decode("utf-8")
        my_dict2 = json.loads(mystr2)
        ordersComplete2 = int(my_dict2['orderCount'])

        mystr3 = pageContent3.content.decode("utf-8")
        my_dict3 = json.loads(mystr3)
        ordersComplete3 = int(my_dict3['orderCount'])

        mystr4 = pageContent4.content.decode("utf-8")
        my_dict4 = json.loads(mystr4)
        ordersComplete4 = int(my_dict4['orderCount'])

        mystr5 = pageContent5.content.decode("utf-8")
        my_dict5 = json.loads(mystr5)
        ordersComplete5 = int(my_dict5['orderCount'])

        mystr6 = pageContent6.content.decode("utf-8")
        my_dict6 = json.loads(mystr6)
        ordersComplete6 = int(my_dict6['orderCount'])
        
        # Append dataframe for recording data
        ordercounts = ordercounts.append({'time':datetime.now(),
                           'location1_cart':'NaN','location1_confirmed':ordersComplete1,
                           'location2_cart':'NaN','location2_confirmed':ordersComplete2,
                           'location3_cart':'NaN','location3_confirmed':ordersComplete3,
                           'location4_cart':'NaN','location4_confirmed':ordersComplete4,
                           'location5_cart':'NaN','location5_confirmed':ordersComplete5,
                           'location6_cart':'NaN','location6_confirmed':ordersComplete6}, ignore_index=True)
       
        # Terminal printout
        print(datetime.now(), ordersComplete1, ordersComplete2, ordersComplete3, ordersComplete4,
              ordersComplete5, ordersComplete6)
        
        # Save CSV of data
        ordercounts.to_csv('F2F_Location_Monitoring_Data_7_28-7_29.csv', index=False)
        
        # Conditions for hiding/closing/re-opening locations
        if ((ordersComplete1 >= 79) & (trigger1 == False)):
            
            # Trigger = True to prevent additional put requests
            trigger1 = True
            # Get request for locations data
            locText = s.get(locUrl, headers=headers)
            # Load data as json dictionary
            locDict = json.loads(locText.text)
            # Location 1 data is stored in element [0]
            stopData1 = locDict['data'][0]
            
            # Modify location 1 data; hide/close
            stopData1['locClosed'] = 'true'
            stopData1['locHide'] = 'true'
            
            # Json put request to locations to close/hide location 1
            stopreq = s.put(stopUrl, json={'locations': stopData1}, headers = headers)
            
            # Terminal printout: status_code 200 = success, text will show details, stopData1 will print
            print(stopreq.status_code)
            print('PUW1 Closed')
            print(stopreq.text)
            print(stopData1)
            
        if ((ordersComplete1 < 79) & (trigger1 == True)):
            
            trigger1 = False
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData1 = locDict['data'][0]
            
            stopData1['locClosed'] = 'false'
            stopData1['locHide'] = 'false'
            
            stopreq = s.put(stopUrl, json={'locations': stopData1}, headers = headers)
            
            print(stopreq.status_code)
            print('PUW1 Opened')
            print(stopreq.text)
            print(stopData1)
            
        if ((ordersComplete2 >= 79) & (trigger2 == False)):
                    
            trigger2 = True
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData2 = locDict['data'][1]
            
            stopData2['locClosed'] = 'true'
            stopData2['locHide'] = 'true'
        
            stopreq = s.put(stopUrl, json={'locations': stopData2}, headers = headers)
    
            print(stopreq.status_code)
            print('PUW2 Closed')
            print(stopreq.text)
            print(stopData2)
            
        if ((ordersComplete2 < 79) & (trigger2 == True)):
            
            trigger2 = False
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData2 = locDict['data'][1]
            
            stopData2['locClosed'] = 'false'
            stopData2['locHide'] = 'false'
            
            stopreq = s.put(stopUrl, json={'locations': stopData2}, headers = headers)
            
            print(stopreq.status_code)
            print('PUW2 Opened')
            print(stopreq.text)
            print(stopData2)
            
        if ((ordersComplete3 >= 79) & (trigger3 == False)):
                    
            trigger3 = True
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData3 = locDict['data'][2]
            
            stopData3['locClosed'] = 'true'
            stopData3['locHide'] = 'true'
        
            stopreq = s.put(stopUrl, json={'locations': stopData3}, headers = headers)
    
            print(stopreq.status_code)
            print('PUW3 Closed')
            print(stopreq.text)
            print(stopData3)
            
        if ((ordersComplete3 < 79) & (trigger3 == True)):
            
            trigger3 = False
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData3 = locDict['data'][2]
            
            stopData3['locClosed'] = 'false'
            stopData3['locHide'] = 'false'
            
            stopreq = s.put(stopUrl, json={'locations': stopData3}, headers = headers)
            
            print(stopreq.status_code)
            print('PUW3 Opened')
            print(stopreq.text)
            print(stopData3)
            
        if ((ordersComplete4 >= 79) & (trigger4 == False)):
                    
            trigger4 = True
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData4 = locDict['data'][3]
            
            stopData4['locClosed'] = 'true'
            stopData4['locHide'] = 'true'
        
            stopreq = s.put(stopUrl, json={'locations': stopData4}, headers = headers)
    
            print(stopreq.status_code)
            print('PUW4 Closed')
            print(stopreq.text)
            print(stopData4)
            
        if ((ordersComplete4 < 79) & (trigger4 == True)):
            
            trigger4 = False
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData4 = locDict['data'][3]
            
            stopData4['locClosed'] = 'false'
            stopData4['locHide'] = 'false'
            
            stopreq = s.put(stopUrl, json={'locations': stopData4}, headers = headers)
            
            print(stopreq.status_code)
            print('PUW4 Opened')
            print(stopreq.text)
            print(stopData4)
            
        if ((ordersComplete5 >= 79) & (trigger5 == False)):
                    
            trigger5 = True
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData5 = locDict['data'][4]
            
            stopData5['locClosed'] = 'true'
            stopData5['locHide'] = 'true'
        
            stopreq = s.put(stopUrl, json={'locations': stopData5}, headers = headers)
    
            print(stopreq.status_code)
            print('PUW5 Closed')
            print(stopreq.text)
            print(stopData5)
            
        if ((ordersComplete5 < 79) & (trigger5 == True)):
            
            trigger5 = False
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData5 = locDict['data'][4]
            
            stopData5['locClosed'] = 'false'
            stopData5['locHide'] = 'false'
            
            stopreq = s.put(stopUrl, json={'locations': stopData5}, headers = headers)
            
            print(stopreq.status_code)
            print('PUW5 Opened')
            print(stopreq.text)
            print(stopData5)
            
        if ((ordersComplete6 >= 79) & (trigger6 == False)):
                    
            trigger6 = True
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData6 = locDict['data'][5]
            
            stopData6['locClosed'] = 'true'
            stopData6['locHide'] = 'true'
        
            stopreq = s.put(stopUrl, json={'locations': stopData6}, headers = headers)
    
            print(stopreq.status_code)
            print('PUW6 Closed')
            print(stopreq.text)
            print(stopData6)
            
        if ((ordersComplete6 < 79) & (trigger6 == True)):
            
            trigger6 = False
            locText = s.get(locUrl, headers=headers)
            locDict = json.loads(locText.text)
            stopData6 = locDict['data'][5]
            
            stopData6['locClosed'] = 'false'
            stopData6['locHide'] = 'false'
            
            stopreq = s.put(stopUrl, json={'locations': stopData6}, headers = headers)
            
            print(stopreq.status_code)
            print('PUW6 Opened')
            print(stopreq.text)
            print(stopData6)
            
        # Conditions for ending loop/request session. MUST MODIFY DATETIME CONDITION BEFORE STARTING
        if (((trigger1 == True) & (trigger2 == True) & (trigger3 == True) & (trigger4 == True) &
            (trigger5 == True) & (trigger6 == True)) | (datetime.now() >= datetime(2020,7,29,12))):
            
            i = 0
            break
        
        # Scrape and print order counts every 60 seconds
        time.sleep(60)

