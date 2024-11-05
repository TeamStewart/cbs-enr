from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
import os
import time
from datetime import datetime

def setup_driver(state, dropbox_path):
    # Set up Chrome options
    chrome_options = Options()
    
    download_directory = os.path.abspath(f'{dropbox_path}/24_general/{state}/raw')
    
    # Specify the download directory and enable automatic downloads
    prefs = {"download.default_directory": download_directory,
             "download.prompt_for_download": False,
             "download.directory_upgrade": True,
             "safebrowsing.enabled": True}
    chrome_options.add_experimental_option("prefs", prefs)

    # Initialize the Selenium webdriver with Chrome options
    # Note: Update the path to chromedriver if necessary or use the Service class as per Selenium's latest practices
    service = Service(executable_path="path_to_chromedriver")  # Update this path
    driver = webdriver.Chrome(options = chrome_options)
    
    return driver, download_directory

def download_wait(download_directory):
    seconds = 0
    dl_wait = True
    while dl_wait and seconds < 60:
        dl_wait = False
        for fname in os.listdir(download_directory):
            if fname.endswith('.crdownload'):
                dl_wait = True
        seconds += 1
    return seconds

def rename_file_with_timestamp(directory):
    files = os.listdir(directory)
    full_paths = [os.path.join(directory, f) for f in files if f.endswith((".csv", ".txt"))]

    if not full_paths:
        return None  # No csv files found

    # Find the most recent file based on last modification time
    most_recent_file = max(full_paths, key=os.path.getmtime)
    
    # Generate a timestamp for renaming
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    new_filename = f"{os.path.splitext(os.path.basename(most_recent_file))[0]}_{timestamp}{os.path.splitext(most_recent_file)[1]}"
    new_path = os.path.join(directory, new_filename)

    # Rename the file
    os.rename(most_recent_file, new_path)

    return new_filename

def get_file(path, county, state, dropbox_path):
    # Ensure the provided path is expanded
    dropbox_path = os.path.expanduser(dropbox_path)
    # Initialize the driver
    driver, download_directory = setup_driver(state, dropbox_path)
    
    # Navigate to the URL
    driver.get(path)
    
    # In Philadelphia County, PA, dynamically click download link
    if state == "PA" and county == "Philadelphia": driver.find_element(By.ID, "MainContent_LinkButton1").click()
    
    # Wait for some time to ensure the file gets downloaded
    download_seconds = download_wait(download_directory)
    
    if download_seconds == 30:
        raise Exception("Download did not complete successfully")
        
    rename_file_with_timestamp(download_directory)
        
    # Close the webdriver
    driver.quit()
