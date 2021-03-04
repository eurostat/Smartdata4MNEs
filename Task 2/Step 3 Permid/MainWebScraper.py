#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 16 08:09:38 2020

# Written by: Fotis Papailias, 2020
# (c) European Commission, Eurostat
# Please do NOT use/reproduce without citing!
#
# Supply of statistical services in statistical methodology 
# Lot 1: Methodological support
# Smart data for multinational enterprises (MNEs)
# Framework Contract No 2018.0086
# Ref. No.: ESTATMET2-000050-6000054239-REQ-01

This file contains the main function which webscrapes all info for all compainies on 

PERMID.org 

Option: set stack_Output = True to get a csv for all companies

"""

# To begin with, we need to import the  necessary python packages
from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
import os
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
import time
import numpy as np
from selenium.common.exceptions import TimeoutException


def WebScraper(urlList,stack_Output = True):
    """
    Input = ID List of companies
    stack_Output =  option to save  the data
    if True it saves one csv for all companies
    if False it saves each company separately.
    
   
    
    """
    
    
    Total_comp = pd.DataFrame() # this accumulates all companies's pages
    OutputDir =  os.path.realpath('Output')
    for index, url_initial in enumerate(urlList['ID_PermID']):
        print('Open webpage company with ID: '  + '1-'+  str(url_initial))
        print('and name: ' + urlList['MNE_Estat_Name'].iloc[index])
        
        #launch url
        url = "https://permid.org/"  + '1-' + str(url_initial)
    
        
                
        # =============================================================================
        # The actual stuff
        # =============================================================================
        # create a new Chrome session
        driver = webdriver.Chrome()
        
   
        # This navigates to the page
        driver.get(url)
        
        # Identify the 'show more'   button (if thre is one ) and expand the page to webscrape all names
        try:
            wait = WebDriverWait(driver, 30) # wait here to find the button
            show_button_success = wait.until(EC.element_to_be_clickable((By.XPATH, '//*[@id="body"]/section/div/div[2]/div[2]/div/div/a[1]')))
            time.sleep(np.random.uniform(1,2)) # 
            show_button_success.click() # click the button
        except TimeoutException:
            print('There is no show more button, just scrape the page as  it is!')
     
        
        # thisi is the name of the class html attribute which has all the information
        classname = 'row matchInformationPage-results ng-scope' 
        
        
        #Selenium hands the page source to Beautiful Soup
        soup_level1=BeautifulSoup(driver.page_source, 'lxml')
        
        # grab the company name's from 'MSE_Estat_Name' column
        companyName  = urlList['MNE_Estat_Name'].iloc[index]

        ### identify the the above class using  Beautiful Soup
        org_table = soup_level1.find("div", attrs={"class": classname})
        
        
        
        ## first table -  Organisation details construction
        
        org_table_data = org_table.find_all("div")  
        
        
        
        # first create a dataframe with the specific attributes that gets all the data
        # for  Organisation Details, Primary Quote and Primary Instrument:
        
        total = org_table_data[0].select('div.col-md-6, div.col.md-6,div.col-md-6.ng-scope, div.col-md-6.ng-binding' )
        
        h = [] # list which stores all the data

        for td in total: # loop through the list and extract the text
            # remove any newlines and extra spaces from left and right
            h.append(td.text.replace('\n', ' ').strip()) #save the text  on 'h' list
        
    
        TableCreator = list(zip(h[::2], h[1::2]))  # create the first table as shown on the site
        df = pd.DataFrame(TableCreator, columns=['Features', 'Organisation_Details']) # add column names
        
        # =============================================================================
        # 2 table construction officers and Directors
        # =============================================================================
        
        
        
        # identify the class which includes the officers and directors
        Officers = soup_level1.find("div", attrs={"class": "col-md-7 col-sm-7"})
        table = Officers.find_all("div") 
        if table: # this handles the case where no officers and directors are available
            headings = [th.get_text() for th in table[0].find("tr")] # grab the headings
            
            # store in a list all the relative information 
            final_results = [i.text if i.find('a') else i.text for i in  table[0].find_all('td')]
          
            # construct the table as show on the website
            data = final_results[::4], final_results[1::4], final_results[2::4], final_results[3::4]
            dataset = [] # empty list which stores the metadata  per name of director
            for row in table[0]:
                dataset.append(row.get_text())
            
            dictionary = {} # create a dictionary with all directors as keys and values their position, title, start date
            for i in range(0, len(data[0])):
                dictionary[data[0][i]] = [data[1][i],  data[2][i],data[3][i] ]
      
           
             # convert dictionary to dataframe to constract the table 
            office_df = pd.DataFrame.from_dict(dictionary,orient ='index', columns= headings[1:])
            # assign  the name of directors as index
            office_df['Features'] =      office_df.index 
            #reset index (to be easier to stack the data later)
            office_df = office_df.reset_index()
            # create a dataframe with columns :1. the name of directors, 2. position, title,  start_date
            # and column their relative attributes
            df_sorted = pd.melt(office_df, id_vars=['Features'], value_vars=headings[1:], var_name = 'Organisation_Details', value_name = 'Info')
            # sort them with an ascending order of name 
            df_sorted = df_sorted.rename_axis('Index').sort_values(by = ['Features', 'Index'], ascending = [True, True])

            # stack both tables
            company_details = pd.concat([df,df_sorted ], axis= 0, ignore_index = True, sort=False)
            

            
        else:
            print('No officers and directors for: ' + str(url_initial))
             # company details just the organisation details
            company_details = df
            
            
            
            
        # create an addtional column with company details
        company_details['Company'] = companyName
        # set as index
        company_details.set_index('Company', inplace=True)
        
        # save per company
        if not stack_Output: 
            company_details.to_csv(os.path.join(OutputDir,  str(url_initial) + '.csv'))
        else:
    # keep the data and  merge with all the other companies
            Total_comp = pd.concat([Total_comp, company_details] , sort  = False)
    

    if stack_Output: 
     # save all companies
      Total_comp.to_csv(os.path.join(OutputDir, 'All_companies_PERMID.csv'))
            
            



# =============================================================================
# Once you import the packages and the function  run the below lines of code
# =============================================================================

# read  the csv file which stores all id's and names of companies 
urlList = pd.read_csv('PermidID.csv', encoding  = 'latin-1')
urlList = urlList.dropna() # drop the ones with no IDs
urlList['ID_PermID'] =  urlList['ID_PermID'].astype(np.int64)

WebScraper(urlList, stack_Output = True)