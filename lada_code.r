packages <- c("data.table", "stringi", "stringr", "rvest", "httr", "xml2", "readr", "tidyverse", "ggmap", 
              "lubridate", "WikidataR", "anytime", "ggplot2", "RgoogleMaps", "stargazer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  
  install.packages(setdiff(packages, rownames(installed.packages())))
}

capture.output(lapply(packages, library, character.only = T), file='NULL')

#to change WD to project folder
setwd("C:/Users/Xiaomi/Documents/plaques_progect")

#to get all the pages
plaques_web <- xml2::read_html('http://encspb.ru/object/2805516545/D_1803401815/')

plaque_object_inventory <- c()

for( i in 1:30 ) {
	url_to_query <- paste0("http://encspb.ru/object/2805516545/D_1803401815/", i)
	page_content <- httr::GET(url_to_query)
	page_dom <- xml2::read_html(page_content$content)
	plaque_objects <- xml_attr(xml2::xml_find_all(page_dom, "//a[@class='wp_td_link link']"), "href")
	plaque_object_inventory <- c(plaque_object_inventory, plaque_objects)}

html_list <- c()
ids <- c()
for(obj in plaque_object_inventory) {

	# Debug: obj <- "/object/2805528494?lc=ru"
	url_to_query <- paste0("http://encspb.ru", obj)

	# Unique ID
	objid <- gsub('[^0-9.-]','', obj)
	ids <- c(ids, objid)

	curr_html_name <- paste0("plaques_file", objid, ".html")
	html_list <- c(html_list, curr_html_name)

	if (!file.exists(curr_html_name)) {
	  httr::GET(url_to_query,
	              write_disk(curr_html_name))}}

plaque_address <- c()
plaque_title <- c()
plaque_content <-  c()
plaque_year <- c()

for (n in html_list) {
  page_content <- read_file(n)
  curr_page <- xml2::read_html(page_content)
  
#column with title
  page_title <- html_nodes(curr_page, "h3")
  pl_title <- html_text(page_title)
  if (length(pl_title) == 0) {
    pl_title <- NA}
  
  plaque_title <- c(plaque_title, pl_title)
  
#column with year, architect, material
  pl_year <- xml2::xml_find_all(curr_page, "//a[@class='inner_object']/text()[preceding-sibling::br[3]][1]")
  pl_year <- xml_text(pl_year)
  if (length(pl_year) == 0) {
    pl_year <- NA}
  plaque_year <-  c(plaque_year, pl_year)

#column with description
  page_desc <- html_nodes(curr_page, ".inner_object")
  pl_description <- html_text(page_desc)
  pl_content <- gsub('\n', '', pl_description)
  pl_content <- gsub( '\\s+', ' ', pl_content)
  if (length(pl_content) == 0) {
    pl_content <- NA}
  plaque_content <- append(plaque_content, pl_content)
  
#column with address
  pl_address <- xml2::xml_find_all(curr_page, "//div[@class='grName']/following-sibling::a[3]")
  pl_address <- xml_text(pl_address[1])
  if (length(pl_address) == 0) {
    pl_address <- NA}
  plaque_address <- c(plaque_address, pl_address[1])
}

#create a dataframe
df_plaques <-  data.frame(
  ID = ids,
  TITLE = plaque_title,
  DESCRIPTION = plaque_content,
  ARCHITECT = plaque_year,
  ADDRESS = plaque_address)


write.csv(new_df, file = 'new_df', row.names = FALSE, fileEncoding = "UTF-8")

#separating_columns
new_df <- df_plaques %>% 
  separate(DESCRIPTION, c("Trash", "Description", "Other"), sep = "([\\�\\�])") 

new_df <- new_df %>%
  separate(TITLE, c("Person", "z"), sep = ",") %>%
  select(-z)

new_df <- new_df %>%
  separate(Person, c("Person", "z"), sep = "\\[") %>%
  select(-z)

new_df <- new_df %>%
  separate(Other, c("Other", "Address"), sep = "������")

new_df <- new_df %>%
  separate(Address, c("Address", "q"), sep = "����������") %>%
  select(-q) 

plaques <- new_df %>%
  separate(Other, c("Year", "Other"), sep = ". ") %>%
  select(-c("ADDRESS", "ARCHITECT", "Other"))

#fixing data types
plaques$ID <- as.character(plaques$ID) 
plaques$Person <- as.factor(plaques$Person)
plaques$Description <- as.character(plaques$Description)
plaques$Year <- as.character(plaques$Year)
plaques$Address <- as.character(plaques$Address)

#NA replacement
ind <- which(is.na(new_df$Address))

plaques <- setDT(plaques)

plaques[ID == "2805524710", Address := "���. ���������� ���./�����-���������, �. 2, ���. �"]
plaques[ID == "2805531115", Address := "21-� ����� �.�./�����-���������, �. 8, ���. �"]
plaques[ID == "2805530415", Address := "���������� ��./�����-���������, �. 19"]
plaques[ID == "2805531580", Address := "������� ��./�����-���������, �. 82"]
plaques[ID == "2805550894", Address := "��������� ��./�����-���������, �. 56"]
plaques[ID == "2805551543", Address := "������������ ��./�����-���������, �. 9"]
plaques[ID == "2805524985", Address := "������� ��./�����-���������, �. 56"]
plaques[ID == "2805551911", Address := "�. �������������� ��./�����-���������, �. 60"]
plaques[ID == "2805545071", Address := "������ ��./�����-���������, �. 18"]
plaques[ID == "2805527011", Address := "�������� ��./�����-���������, �. 13"]
plaques[ID == "2805526998", Address := "�������� ��./�����-���������, �. 13"]
plaques[ID == "2805527030", Address := "���������� ��./�����-���������, �. 33"]
plaques[ID == "2805545161", Address := "��������� ��./�����-���������, �. 281"]
plaques[ID == "2805527143", Address := "������ ��./�����-���������, �. 47"]
plaques[ID == "2805524999", Address := "������� ��./�����-���������, �. 7"]
plaques[ID == "2805545313", Address := "�������� ���. �/� ��"]
plaques[ID == "2805527261", Address := "�����-���������, ������ ��., 1."]
plaques[ID == "2805528644", Address := "�����-���������, �/� ��. ��������"]
plaques[ID == "2805528218", Address := "���������� ���. �/� ��"]
plaques[ID == "2805527421", Address := "��������� ������ ���., 223�225."]
plaques[ID == "2805527594", Address := "����������, ���. ������, ��. ����������, �. 3."]
plaques[ID == "2805528048", Address := "������������������� ��./�����-���������, �. 23"]
plaques[ID == "2805528068", Address := "��������������, ������������� ��.59"]
plaques[ID == "2805528076", Address := "��������������, ������������� ��.59"]
plaques[ID == "2805528192", Address := "����������, ���. ��������, ������ � ������ ������, �. 3, ����� �"]
plaques[ID == "2805545966", Address := "������ ��./�����-���������, �. 15"]
plaques[ID == "2805525156", Address := "���������� ��./�����-��������� 1/3"]
plaques[ID == "2805525759", Address := "��������� ��./�����-���������, �. 33"]
plaques[ID == "2805547031", Address := "������ ��./�����-���������, �. 18"]
plaques[ID == "2805553878", Address := "���������� ��., 30"]
plaques[ID == "2805547577", Address := "�.�., 21- �����, 8�"]
plaques[ID == "2805555122", Address := "��. �����������/�����-���������, �. 18"]
plaques[ID == "2805555302", Address := "������� ��./�����-���������, �. 85"]
plaques[ID == "2805559085", Address := "���������� ��./�����-��������� 1/3"]
plaques[ID == "2805548815", Address := "21-� ����� �.�./�����-���������, �. 8, ���. �"]
plaques[ID == "2805548978", Address := "7-� ����� �.�./�����-���������, �. 2"]
plaques[ID == "2805549217", Address := "��������� ��./�����-���������, �. 123"]


#get coordinates
pl_longitude <- c()
pl_latitude <-  c()

library(ggmap)
register_google(key = "AIzaSyApbGcAM8vQ13zp-sZUm0skQjRBWwCdVe8")

for (row in 1:nrow(plaques)) {
  curr_addr <- plaques[row, "Address"]
  
  if (!is.na(curr_addr)) {
    rawgeo <- geocode(as.character(curr_addr), output='all')
    curr_long <- format(rawgeo$results[[1]]$geometry$location$lng, digits=20)
    curr_lat <- format(rawgeo$results[[1]]$geometry$location$lat, digits=20)
    pl_longitude <- c(pl_longitude, curr_long)
    pl_latitude <-  c(pl_latitude, curr_lat)
  }
  else {
    pl_longitude <- c(pl_longitude, NA)
    pl_latitude <- c(pl_latitude, NA)
  }
}

plaques <- cbind(plaques, pl_longitude)
plaques <- cbind(plaques, pl_latitude)

# plaques[, Pl_longtitude := pl_longitude]
# plaques[, Pl_latitude := pl_latitude]




#manually fixing Persons
plaques[ID == "2805530337", Person := "��������� �. �."]
plaques[ID == "2805531128", Person := "�������� �. �."]
plaques[ID == "2805551198", Person := "������ �. �."]

#manually fixing plaque dates
plaques[ID == "2805531090", Year := "�� 1978"]
plaques[ID == "2805549446", Year := "1949"]
plaques[ID == "2805531187", Year := "�� 1977"]
plaques[ID == "2805531271", Year := NA]
plaques[ID == "2805526374", Year := "1929�1930"]
plaques[ID == "2805531493", Year := NA]
plaques[ID == "2805530429", Year := "1969"]
plaques[ID == "2805531580", Year := "1988"]
plaques[ID == "2805531592", Year := "�� 1977"]
plaques[ID == "2805531635", Year := "1960-�"]
plaques[ID == "2805531801", Year := "1949"]
plaques[ID == "2805550894", Year := "1988"]
plaques[ID == "2805531900", Year := "�� 1977"]
plaques[ID == "2805531942", Year := "�� 1976"]
plaques[ID == "2805531975", Year := "1950-�"]
plaques[ID == "2805526517", Year := "1975"]
plaques[ID == "2805551100", Year := "1955"]
plaques[ID == "2805551373", Year := "1911"]
plaques[ID == "2805551404", Year := "1911"]
plaques[ID == "2805551514", Year := "19-� ���"]
plaques[ID == "2805551543", Year := "1991"]
plaques[ID == "2805551563", Year := "1955"]
plaques[ID == "2805524951", Year := "1910-�"]
plaques[ID == "2805524970", Year := "1990-�"]
plaques[ID == "2805544875", Year := "1980-�"]
plaques[ID == "2805544896", Year := "1960-�"]
plaques[ID == "2805544968", Year := "1960-�"]
plaques[ID == "2805545014", Year := "1983"]
plaques[ID == "2805545060", Year := "�� 1978"]
plaques[ID == "2805526888", Year := "1948"]
plaques[ID == "2805526845", Year := "1940-�"]
plaques[ID == "2805526836", Year := "1940-�"]
plaques[ID == "2805527021", Year := "1983"]
plaques[ID == "2805527042", Year := "1920-�"]
plaques[ID == "2805545161", Year := "1930-�"]
plaques[ID == "2805527143", Year := "1934"]
plaques[ID == "2805552269", Year := NA]
plaques[ID == "2805527232", Year := "1985"]
plaques[ID == "2805545430", Year := "�� 1977"]
plaques[ID == "2805527278", Year := "1930-�"]
plaques[ID == "2805545596", Year := "�� 1977"]
plaques[ID == "2805552747", Year := "1955"]
plaques[ID == "2805545650", Year := "1978"]
plaques[ID == "2805525089", Year := "�� 1941 �."]
plaques[ID == "2805527508", Year := "1940-�"]
plaques[ID == "2805527957", Year := "1960"]
plaques[ID == "2805527967", Year := "1960"]
plaques[ID == "2805528332", Year := "1967"]
plaques[ID == "2805527457", Year := "1930-�"]
plaques[ID == "2805527499", Year := "1940-�"]
plaques[ID == "2805545966", Year := "1997"]
plaques[ID == "2805525156", Year := "�� 1917"]
plaques[ID == "2805546126", Year := "1960-�"]
plaques[ID == "2805546158", Year := "1960-�"]
plaques[ID == "2805525615", Year := NA]
plaques[ID == "2805546209", Year := "1914-1915"]
plaques[ID == "2805529015", Year := "1950-�"]
plaques[ID == "2805546443", Year := "1960-�"]
plaques[ID == "2805546364", Year := "1930-�"]
plaques[ID == "2805525701", Year := "1998"]
plaques[ID == "2805525759", Year := "1905"]
plaques[ID == "2805525759", Description := "������������ ����� � ��������� ����������� ��������� �������, �������� �� ���� ��� � 1805 �� 1905 ����"]
plaques[ID == "2805525770", Year := "1910-�"]
plaques[ID == "2805525788", Year := "1970-�"]
plaques[ID == "2805525798", Year := "1910-�"]
plaques[ID == "2805547192", Year := NA]
plaques[ID == "2805547444", Year := "�� 1977"]
plaques[ID == "2805547320", Year := "1997"]
plaques[ID == "2805547282", Year := "1945"]
plaques[ID == "2805529139", Year := NA]
plaques[ID == "2805547566", Year := "�� 1977"]
plaques[ID == "2805554484", Year := "1985"]
plaques[ID == "2805554527", Year := "1958"]
plaques[ID == "2805529200", Year := NA]
plaques[ID == "2805547670", Year := "1927"]
plaques[ID == "2805554639", Year := "1910-�"]
plaques[ID == "2805526040", Year := "1950"]
plaques[ID == "2805529314", Year := NA]
plaques[ID == "2805555213", Year := "1986"]
plaques[ID == "2805529368", Year := "1965"]
plaques[ID == "2805529397", Year := NA]
plaques[ID == "2805548566", Year := "1960-�"]
plaques[ID == "2805548586", Year := "1998"]
plaques[ID == "2805526162", Year := "1913-1914"]
plaques[ID == "2805559095", Year := NA]
plaques[ID == "2805559085", Year := "1890-�"]
plaques[ID == "2805548764", Year := "1960-�"]
plaques[ID == "2805548805", Year := "1960-�"]
plaques[ID == "2805559197", Year := "1929"]
plaques[ID == "2805529510", Year := "1970-�"]
plaques[ID == "2805548842", Year := "1980-�"]
plaques[ID == "2805548978", Year := "1993"]
plaques[ID == "2805548962", Year := NA]
plaques[ID == "2805549011", Year := "�� 1977"]
plaques[ID == "2805549020", Year := "�� 1983"]
plaques[ID == "2805549232", Year := "�� 1977"]

plaques[, Other := NULL]
plaques[, Names :=NULL]

plaques[, ID:=as.character(ID)]
plaques[, Person:=as.factor(Person)]
plaques[, Description:=as.character(Description)]
plaques[, Year:=as.factor(Year)]
plaques[, Address:= as.character(Address)]
plaques[, pl_latitude:=as.factor(pl_latitude)]
plaques[, pl_longitude:=as.factor(pl_longitude)]

write.csv(plaques, file = 'plaques_0504', row.names = FALSE, fileEncoding = "UTF-8")

plaques <- read.csv("plaques_0504.csv", encoding = "UTF-8")

setDT(plaques)

plaques <- plaques %>%
  select(-c(1))

#adding 19 plaques of Alexander II

#creating dataset
id <- 2805524710
person <- "��������� II"
description <- "some text"
year <- "1907"
address <- "���. ���������� ���./�����-���������, �. 2, ���. �"
alex_pl_longitude <- "30.3289"
alex_pl_latitude <- "59.9401"
material <- "������"
is_sculptor <- "FALSE"

for (i in 1:18) {
  id <- c(id, paste0("2805524710", i))
  person <- c(person, "��������� II")
  description <- "some text"
  year <- c(year, "1907")
  address <- c(address, "���. ���������� ���./�����-���������, �. 2, ���. �")
  alex_pl_longitude <- c(alex_pl_longitude, 30.3289)
  alex_pl_latitude <- c(alex_pl_latitude, 59.9401)
  material <- c(material, "������")
  is_sculptor <- c(is_sculptor, FALSE)
}

#creating dataframe
alex_df <- data.frame(
  ID = id,
  Person = person,
  Description = description,
  Year = year,
  Address = address,
  pl_latitude = alex_pl_latitude,
  pl_longitude = alex_pl_longitude,
  Material = material,
  Is_sculptor = is_sculptor
)


#filling in descriptions
setDT(alex_df)
alex_df[ID == "2805524710", Description := "17 ������ 1818 ����. 
        �������� �������� ����� ���������� ����������� � ���������� ������. 
        ��� �������� �� �������� ������ ���, �� �������� �������� ������� �����, 
        �� ����� ������� �� �������, ���������� �� ������ �������!� ���������. 1818 �."]
alex_df[ID == "28055247101", Description := "12 ������� 1852 ����. �������������� �������� ����� ���������� ����������� 
        ����������� ��������. 16 ������ 1841 ����. �������������� ���������� ���������� � �������� ����� ���������� 
        ����������� � ������� ������� ������ ��������������, ���������� ������-�������������"]
alex_df[ID == "28055247102", Description := "19 ������� 1855 ����. ���������� �� ������� �������� ���������� ���������� II.
        �... ������� �� ��������������� ��� ������� ���������� ������� � ������������ � ��� ������� ��������� 
        � �������� ��������� ������������ ����� ����� �������� ����������������� ��� ����, �������� ��������� ���� ����� 
        ������ ������ ����� ������������� ��������� ������. �� �����������, ������������������� ���������� ��� 
        � ���� �������� �������� ����������� �������� ������ �� ������ ������� ���������� � �����, 
        �� ����������� ����� ��� ���������� ������� � ���� ����������� ����� ���������������� �����, ���������, 
        ���������� ��������������� � ������������ ������ ��������� ���������"]
alex_df[ID == "28055247103", Description := "26 ������� 1856 ����. ��������� ����������� � ������������� 
        �������� ���������� ���������� ����������� � ���������� ����������� ����� ������������� � ������. 
        �� ��� ������������� ����, �����, �������� ������������� ����������, �� ��������� �� ���� ����� 
        ����� �������, ������ ����� ������ ����, ��� ������, ������������� �������� ��� ������. �������� 
        ��� ��������� ������ ����������� ����, ������������� ���� � ����� ��� ���������� ������ �� 
        ��������������� �������, ����� ����������, ������ ����� ������ � ��������� ����� �����������, 
        ���������� ���� ������������� � ��������� � ������� �������. �� �� �����, � ��� ������, �� ���������� 
        � � ������������ � �������� ������� �������� ���, �������������� ���������� �����������, �� � ��������� 
        ������� �������� � ������ ���������������� �������������, ������������ ����������� ������ ��������� ����� 
        ���� ��������� � �������� � ���������...� ���������"]
alex_df[ID == "28055247104", Description := "19 ����� 1856 ����. ���������� ����, ����������� ����� ��������� 
        ����� 1853�1856 ��. ��������, �������������� ������, ����������� ������ � ������� ���� ����� ���, 
        ������������... ��� ������ ��������� �������� ������ ������������ ������, �� ������������ � 
        ���������������� �� ���������� ���������������, ������ � ������� �� ��������� � ����� ��, �� ����������� 
        ������� � � ����� ����� ���������� � ����������� � ������ �������� ������������, � ������ ��� ����� 
        �������, ��� ���� ����� ������������, ���� ����� �������������������, �� ������������ � ���� ������ ������
        ��������. �������, � ��� ���� ������ �������� ������� ����, ���� ������������ ����, ������ ���, �������� 
        ������, �� ��������� � �������� ����� � ����� ������������ ��������������, ��� ��������� ����� ������� � 
        �������...� ���������"]
alex_df[ID == "28055247105", Description := "1858 � 1860 ��. ������������� � ������ ��������� � ������������ ����. 
        ��������� ������� 16 ��� 1858 ����. ��������� ������� 2 ������ 1860 ����"]
alex_df[ID == "28055247106", Description := "1859 � 1864 ��. ��������� �������. ������ ������ � �������� ������ 
        25 ������� 1859 ����. ��������� ���������� ����� 25 ��� 1864 ����"]
alex_df[ID == "28055247107", Description := "19 ������� 1861 ����. ������������ �������� �� ���������� �����������. 
        ������ ���� �������� ���������, ������������ �����, � ������� � ���� ����� ������������� �� ���� ��������� ����, 
        ����� ������ ��������� ������������ � ����� �������������...� ���������"]
alex_df[ID == "28055247108", Description := "22 ��� 1862 ����. ��������� � ��������������� ������� � ���������� 
������ � �� ������������� �� �� �������� ��������.
1862�1866 ��. �������������� ���������������� ��������. �������� ���� �������� ����� � ����������"]
alex_df[ID == "28055247109", Description := "17 ������ 1863 ����. ����������� �������� ���������"]
alex_df[ID == "280552471010", Description := "20 ������ 1864 ����. �������� ������. ����������� ��� �������, 
        �� �������, ��� ��� ������ ������������� ������� ������ ��������� � ������ ��� ������ ������, ���������� 
        � ������ ��� ���� ��������� �����, ��������� �������� ������, ���� �� ���������� ����������������� � ������ 
        ��������� � ������ ����� �� �������� � ������, ��� ����� ���������� ������������ �������������� � ������� 
        ������ ���� ���������� ������������� �������� ���� � ������� �� ������� �� �������...� ���������"]
alex_df[ID == "280552471011", Description := "19 ������� 1864 ����. ���������� ���� �������� ������� ���������. 
        ����� ����������� ����� ��� ���� � ��� ���, ��� � ���� 19 ������� 1861 ���� �� ������ �������� �
        ��������� �� ���������� �������� � ������. � � ������� �������� ��������� ���� �� ���������
        ����������� ���������� ������ �������� ������, ����� ����������� ��������� ������� � �������� 
        ��������������� ������� ��� �������� �������. �� ��������� ��� ���� ����� ������� � ���������� 
        �������, ��� ���� ����� ������������ �� ��������������. �� ����� ��� �������������� �� ���������� 
        ������������� ���� ������ ����������� � �������������, ���������� ����� �� ���� ����� ��������� 
        ������� ���������� ������� ������ ����������� ������� � ����������� �������� ���������"]
alex_df[ID == "280552471012", Description := "1 ������ 1864 ����. ��������� � ������� �����������.
16 ���� 1870 ����. ��������� ���������. 6 ������ 1865 ����. ����� � �������������� ������ ��������� ����������"]
alex_df[ID == "280552471013", Description := "�������� �����������. �� ���������� ������� ���� � ����� ����� 
������ � ������� ��������� ��� �������� �� ���� ��������� �����������, ���� � ��� �������� ���� ������� ������ 
� ����������� ��� ������������ �����, �� ������� �������� ������������ ���������. ��������� � ��������� �������� 
        �������� 1864 �. ��������� ������� � ����������� ��������� 1872 �. ������ �������� � ����������� 1864�1871 ��. 
        ������ �������� ������ 1872 �. ��������������� ����� 1863 �. ��������� � �������� �������� �����������"]
alex_df[ID == "280552471014", Description := "1 ������ 1874 ����. �������� �������� ����������. 
        �...���� ����������� �� � ����� ����������� �����, �� ��������������� � ������������ � ���������� 
        ��� ���������, ����������� ������� �������� ���� �����, ����� ���� ������ ��������� ���������� 
        ����� ����� ������, ����� ��� ��� �������� ������ � ��������� ����������� �� ��� ������ ����... 
        ��������... ����� � �������� ���������� � �������� ��������� ����� ������ ������� ���� ��� ������� 
        � ����������� ���������� ����������� �� ��� ������������, �� �� ����� ��������� ��������� �� �����, 
        ������� ��������� ��������� �� ��� ���� ������������. �� �� ����, ��� �� ������ �� ��� ��� ������ 
        ������� ����� � ������ �������, ������������ ��� �� ����, �������� ����� ������ � ������� ����� ������� 
        ����������� � ������������� ����������� ��������...� ���������"]
alex_df[ID == "280552471015", Description := "19 ������� 1870 ����. ����������� ������� ����� ��������� ���� �� ������ ����"]
alex_df[ID == "280552471016", Description := "���������� ������� ���� 1860�1881 ��. ������ ������� ������ ��������. ������ ��� 
        ���������� �������� � �������. ������ ������� ����������� ������ ��������. ������ ��������� ��������� ���-���� 
        � ������. ��� � ���-�������. ����� ������������� ����� � ������� ������. ��� �� ������������� ������� � ������� 
        ��. ������� ����, ������� �������. ������ ���������� �������� ������. ��������� ������ ������� ������ �������. 
        ������������� � ��������� ������ ����������� �������. ������ ������� �������� ����-����"]
alex_df[ID == "280552471017", Description := "19 ������� 1878 ����. ���������� ���� � �.-�������.
        ������������� � ������� ������������ ������� ����������. ������������� ����� � ������. 
        ������������� �������, ������ � ����������. ������������ ��������"]
alex_df[ID == "280552471018", Description := "����� �� ������������ ���������� ��������. 
        12 ������ 1877 � 19 ������� 1878. �� ����, ��� ��� ������ ������ �� ���� ��������� 
        �������� ������� � ���������� ����� ������ �� ���� � �� �������������... ��� ������ 
        ��������� �� ��� ������, ����� � ����� ��� ������ � ����� ������ ���� ���������... 
        �� ������� ��� ��� ��������� ���� ������ ��������� (�� ����, ������������� ����������� 
        ����������� II � ����������� ������ 30 ������� 1876 �.) ������ ��������. ��������� ����� 
        ����� � ������ � � �������. ������ �������. ������ ��������. ������ ���������� �������: 
        ������������ � �����������. ������ �����. ��������� ����� �������-���� �� ����������� �������. 
        ������ ������ � �����. ����-�����. ������ ������� �����. ������� ������. �������� ��� ��. 
        ���������� � �����. ������� ����� �������: ���������, ���. �������, �������, ��������� ���������. 
        �����. �������. �����������. ������� ������������. ������� ��������"]

#fixing data types
alex_df[, ID:=as.character(ID)]
alex_df[, Person:=as.factor(Person)]
alex_df[, Description:=as.character(Description)]
alex_df[, Year:=as.factor(Year)]
alex_df[, Address:= as.character(Address)]
alex_df[, pl_latitude:=as.factor(pl_latitude)]
alex_df[, pl_longitude:=as.factor(pl_longitude)]

write.csv(alex_df, file = "alex_df.csv", row.names = FALSE, fileEncoding = "UTF-8")
alex_df <- read.csv('alex_df.csv', fileEncoding = "UTF-8")

#binding datasets

ll <- read.csv("plaques0404.csv", fileEncoding = "UTF-8")
ll <- ll[1:(nrow(ll)-20),]

plaques <- cbind(plaques, ll$pl_latitude)
plaques <- cbind(plaques, ll$pl_longitude)

setnames(plaques, "pl_longitude", "pl_latitude")
setnames(plaques, "V2", "pl_longitude")

#switching places to make a standard lat-long format
plaques <- plaques[ , c(1, 2, 3, 4, 5, 8, 9, 6, 7)]

#getting rid of the row that is being substituted by 19 plaques
new_plaques <- new_plaques[-6,]
#bind
new_plaques <- rbind(plaques, alex_df, use.names=TRUE, stringsAsFactors=FALSE, sort=TRUE, fill=TRUE)


#write.csv(new_plaques, file = "plaques_upd.csv", row.names = FALSE, fileEncoding = "UTF-8")
#new_plaques <- read.csv('plaques_upd.csv', encoding = 'UTF-8')

setDT(new_plaques)

str(new_plaques)
summary(new_plaques)

#removing extra space between initials
new_plaques$Person <- as.character(new_plaques$Person)
new_plaques$Person <- gsub("\\. ", ".", new_plaques$Person)
new_plaques$Person <- as.factor(new_plaques$Person)
summary(new_plaques)

setDT(new_plaques)
new_plaques[, x := NULL]

#top 20 most plaqued people
head(setorderv(new_plaques[, .N, by = "Person"], "N", -1)[], 20)

#but for how long the connection between a person and a place lasted?
new_plaques[, how_long := gsub("[^[:digit:], [:space:], \\�]", "", Description)]
new_plaques[, how_long := str_squish(how_long)]

##what types of connection are present?
new_plaques

#extract actions into a variable
new_plaques[, what_did := NA]

new_plaques[, what_did := as.character(what_did)]

for (row in 1:nrow(new_plaques)) {
  if (grepl("���\\s+�\\s+�������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "��� � �������"
  }
  else if (grepl("������|�������|�������|��������|�����������|�������������|������������|���������|�������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "������"
  }
  else if (grepl("������\\s+�������|������|��������|�������|�������|��������|������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "������"
  }
  else if (grepl("������|�����\\s+���|�������\\s+�\\s+�����|���������\\s+���|�����|���������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "�������� � �����"
  }
  else if (grepl("����|�����|���������|���������� �����|����|�������|��� ������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "����"
  }
  else if (grepl("��������|���������|��������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "���������"
  }
  else if (grepl("���|����|��������|��������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "���"
  }
  else if (grepl("�������|�������|������|��������|����������|�������|����������|�����
                 ���������|��������\\s+�������|��������\\s+������������|�����", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "�������"
  }
  else if (grepl("��������|��������|�����������|��������|����������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "��������"
  }
  else if (grepl("�����|�����������|��������������|����������\\s+�\\s+���������|����������\\s+�\\s+���������|
                 ����������|�������|����������|����������//s+�������|���������|�������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "�����"
  }
  else if (grepl("���������|��������|����������|���������|��������", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "���������"
  }
  else if (new_plaques[row, 'Person'] == "��������� II") {
    new_plaques[row, 'what_did'] = "����"
  }
  else
    new_plaques[row, 'what_did'] = "������"
}

#to check unmatching cases
na_list <- which(is.na(new_plaques$what_did))
na_list
for (i in na_list) {
  print(new_plaques[i, "Description"])
}

#to make a table with top 20 connections
new_plaques[, what_did:=as.character(what_did)]
head(setorderv(new_plaques[, .N, by = "what_did"], "N", -1)[], 20)

#trying to query wikidata
# for (row in 1:nrow(new_plaques)) {
#     #new_plaques[row, 'Person'] <- sub('�\\s', '\\s', ignore.case = F)
#     #new_plaques[row, 'Person'] <- sub('���\\s', '��\\s', ignore.case = F)
#     if (grepl("�", new_plaques[row, 'Person'])) {
#       print('ok')
#     }
# }
# test <- find_item("�����")
#test

#let's look at the map

write.csv(new_plaques, file = "plaques_upd.csv", fileEncoding = "UTF-8")


p <- read.csv("plaques0404.csv", fileEncoding = "UTF-8")


plaques <- plaques %>%
  separate(Person, c("Person", "z"), sep = "\\[") %>%
  select(-z)

plaques <- plaques %>%
  #separate(Other, c("Year", "Other"), sep = ". ") %>%
  select(-c("ADDRESS", "ARCHITECT"))

plaques <- plaques %>%
  separate(Other, c("Other", "Names"), sep = "�������")

plaques <- plaques %>%
  separate(Other, c("Other", "Architect"), sep = "���.")

plaques <- plaques %>%
  separate(Other, c("Year", "Other"), sep = ". ", extra = "merge")

plaques <- plaques %>%
  unite("Other", Other:Architect, remove = TRUE)

plaques$Other <- tolower(plaques$Other)

setDT(plaques)
plaques[, Material := NA]
plaques[, Is_sculptor := NA]

plaques[, Material := as.character(Material)]

for (row in 1:nrow(plaques)) {
  if (grepl("������", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "������"
  }
  else if (grepl("������", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "������"
  }
  else if (grepl("������", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "������"
  }
  else if (grepl("������", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "������"
  }
  else if(grepl("������", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "������"
  }
  if (grepl("���������", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Is_sculptor'] = T
  }
}

write.csv(new_plaques, file='plaques_almost', fileEncoding = 'UTF-8')

ppl <- as.data.table(str_split_fixed(new_plaques$Person, "\\s", 2))
ppl <- cbind(ppl, as.data.table(str_split_fixed(ppl$V2, "[.]", 2)))

setnames(ppl, old = "V1", new = "surname")[]
setnames(ppl, old = "V2", new = "initials")[]
setnames(ppl, old = "V1", new = "init_1")[]
setnames(ppl, old = "V2", new = "init_2")[]


# Load
spb_persons_wikidata <- fread("spb_persons_wikidata_raw.csv", encoding = "UTF-8")
# Unique entries
spb_persons_wikidata <- unique(spb_persons_wikidata, by = "human")
# Proper dates
spb_persons_wikidata[, birthdate := ymd_hms(birthdate)]
spb_persons_wikidata[, deathdate := ymd_hms(deathdate)]

# Remove URI parts
spb_persons_wikidata[, human := gsub("http://www.wikidata.org/entity/", "", human, fixed = T)]

#filtering dataset
#by bearth/death date
spb_persons <- spb_persons_wikidata[spb_persons_wikidata[,birthdate < "1940-01-01"]]
spb_persons <- spb_persons_wikidata[spb_persons_wikidata[,deathdate < "1999-01-01"]]

#removing non cyrillic names
spb_persons <- spb_persons[spb_persons[, ! grepl("\\d", humanLabel)]]
spb_persons <- spb_persons[spb_persons[, grepl("[��������������������������������]", humanLabel, ignore.case = T)]]

#saving dataset
write.csv(spb_persons, file='spb_persons_wiki', fileEncoding = 'UTF-8')

#dataset for names
ppl_big <- as.data.table(str_split_fixed(spb_persons$humanLabel, "\\s", 3))

#creating columns in dataset with plaques
new_plaques[, Name := NA]
new_plaques[, Wiki := NA]
new_plaques[, BirthDate := NA]
new_plaques[, DeathDate := NA]
new_plaques[, Gender := NA]
new_plaques[, Wiki := as.character(Wiki)]
new_plaques[, Name := as.character(Name)]
new_plaques[, BirthDate := as.character(BirthDate)]
new_plaques[, DeathDate := as.character(DeathDate)]
new_plaques[, Gender := as.character(Gender)]

#to add info from wiki df to plaques df
for (row in 1:nrow(ppl)) {
  for (big_row in 1:nrow(ppl_big)) {
    if ((grepl(substr(ppl[row, 'surname'], 1, 4), ppl_big[big_row, 'V1'], fixed = T) &&
        grepl(substr(ppl[row, 'init_1'], 1, 1), ppl_big[big_row, 'V2'], fixed = T) &&
        grepl(substr(ppl[row, 'init_2'], 1, 1), ppl_big[big_row, 'V3'], fixed = T)) ||
        (grepl(substr(ppl[row, 'surname'], 1, 4), ppl_big[big_row, 'V1'], fixed = T) &&
         grepl(substr(ppl[row, 'init_1'], 1, 1), ppl_big[big_row, 'V3'], fixed = T) &&
         grepl(substr(ppl[row, 'init_2'], 1, 1), ppl_big[big_row, 'V2'], fixed = T)) ||
        (grepl(substr(ppl[row, 'surname'], 1, 4), ppl_big[big_row, 'V3'], fixed = T) &&
         grepl(substr(ppl[row, 'init_1'], 1, 1), ppl_big[big_row, 'V1'], fixed = T) &&
         grepl(substr(ppl[row, 'init_2'], 1, 1), ppl_big[big_row, 'V2'], fixed = T)) ||
        (grepl(substr(ppl[row, 'surname'], 1, 4), ppl_big[big_row, 'V3'], fixed = T) &&
        grepl(substr(ppl[row, 'init_1'], 1, 1), ppl_big[big_row, 'V2'], fixed = T) &&
        grepl(substr(ppl[row, 'init_2'], 1, 1), ppl_big[big_row, 'V1'], fixed = T))) {
      new_plaques[row, 'Name'] = paste(ppl_big[big_row, 'V1'], ppl_big[big_row, 'V2'], ppl_big[big_row, 'V3'], sep=" ")
      new_plaques[row, 'Wiki'] = spb_persons[big_row, 'human']
      new_plaques[row, 'BirthDate'] = spb_persons[big_row, 'birthdate']
      new_plaques[row, 'DeathDate'] = spb_persons[big_row, 'deathdate']
      new_plaques[row, 'Gender'] = spb_persons[big_row, 'genderLabel']
      print(ppl[row])
      print(ppl_big[big_row])
      break
    }
  }
}

#fixing date format
new_plaques[, BirthDate := anytime(BirthDate)]
new_plaques[, DeathDate := anytime(DeathDate)]

write.csv(new_plaques, file="pplplaques.csv", fileEncoding = "UTF-8")
setDT(new_plaques)

new_plaques <- fread('pplplaques.csv', encoding = "UTF-8")

new_plaques[, BirthDate := as.Date(BirthDate)]
new_plaques[, DeathDate := as.Date(DeathDate)]


new_plaques[, V1 := NULL]

new_plaques <- new_plaques[ , c(1, 2, 3, 12, 4, 5, 6, 7, 8, 9, 10, 11, 14, 15, 16, 13)]

#to clean unmatching data
#sources
op_1 <- c(12683, 12683, 12677, 12673, NA, NA, 20218, 11863, NA, NA, 13755, 13755, 13755,
          NA, NA, NA, 10636, 14912, 10379, 10124, 19360, NA, NA, NA, NA, 8431, 20621, NA, NA, 18143,
          22337, 22337, 17440, 17440, NA, 20421, 6902, 15006, 15006, 13269, 5, 5,
          5, 5, NA, 21465, NA, 14007, NA, NA, NA, NA, 15550, NA, NA, NA, NA, 18577)
#destinations
op_2 <- c(9, 10, 11, 12, 18, 42, 75, 78, 79, 107, 114, 115, 116, 120, 132, 133,
          178, 191, 202, 220, 361, 372, 376, 510, 511, 378, 519, 545, 546, 554,
          588, 589, 590, 591, 602, 605, 608, 609, 610, 613, 614, 615, 616, 617, 
          631, 708, 725, 726, 755, 760, 761, 778, 801, 835, 836, 881, 882, 883)

#matching cases
for (row in 1:length(op_1)) {
  if (is.na(op_1[row])) {
    new_plaques[op_2[row], 'Name'] = NA
    new_plaques[op_2[row], 'BirthDate'] = NA
    new_plaques[op_2[row], 'DeathDate'] = NA
    new_plaques[op_2[row], 'Gender'] = NA
    new_plaques[op_2[row], 'Wiki'] = NA
  }
  else {
    new_plaques[op_2[row], 'Name'] = spb_persons_wikidata[op_1[row], 'humanLabel']
    new_plaques[op_2[row], 'BirthDate'] = spb_persons_wikidata[op_1[row], 'birthdate']
    new_plaques[op_2[row], 'DeathDate'] = spb_persons_wikidata[op_1[row], 'deathdate']
    new_plaques[op_2[row], 'Gender'] = spb_persons_wikidata[op_1[row], 'genderLabel']
    new_plaques[op_2[row], 'Wiki'] = spb_persons_wikidata[op_1[row], 'human']
  }
}

#Kalinin
for (n in 1:15) {
  new_plaques[254 + n, 'Name'] = NA
  new_plaques[254 + n, 'BirthDate'] = NA
  new_plaques[254 + n, 'DeathDate'] = NA
  new_plaques[254 + n, 'Gender'] = NA
  new_plaques[254 + n, 'Wiki'] = NA
}

#AlexanderII
for (n in 1:19) {
  new_plaques[886 + n, 'Name'] = spb_persons_wikidata[730, 'humanLabel']
  new_plaques[254 + n, 'BirthDate'] = spb_persons_wikidata[730, 'birthdate']
  new_plaques[254 + n, 'DeathDate'] = spb_persons_wikidata[730, 'deathdate']
  new_plaques[254 + n, 'Gender'] = spb_persons_wikidata[730, 'genderLabel']
  new_plaques[254 + n, 'Wiki'] = spb_persons_wikidata[730, 'human']
}

new_plaques[, Wiki := gsub("http://www.wikidata.org/entity/", "", Wiki, fixed = T)]

write.csv(new_plaques, file="wiki_plaques.csv", fileEncoding = "UTF-8")
new_plaques <- fread("wiki_plaques.csv", encoding = "UTF-8")

#fixing types 
#Is_sculptor fixed
new_plaques[is.na(Is_sculptor), Is_sculptor := FALSE]
summary(new_plaques)

stargazer(new_plaques, type = "text", summary = T, out = "plaques_summary_stat.txt", summary.stat = c("n", "mean", "sd", "median", "min", "max"))
#converting to POSIX 
new_plaques[, BirthDate := anytime::anytime(BirthDate)]
new_plaques[, DeathDate := anytime::anytime(DeathDate)]
#Y-m-d
new_plaques[, BirthDate := anytime::anydate(BirthDate)]
new_plaques[, DeathDate := anytime::anydate(DeathDate)]


#how many people didn't live, work, died or were born in SPb based on Wikidata
no_wiki <- new_plaques[new_plaques[, is.na(new_plaques$Name)]]
no_wiki_unique <- no_wiki[!duplicated(no_wiki[,Person]), ]
nrow(no_wiki_unique)

#analyse plaquing patterns based on time period

new_plaques[, Pl_Period := "unknown period"]

#clean the Year column more
new_plaques[X == "808", Year := "1914"]
new_plaques[X == "145", Year := "1970"]
new_plaques[X == "513", Year := "1915"]
new_plaques[X == "28", Year := "1975"] 
new_plaques[X == "62", Year := "1998"] 
new_plaques[X == "508", Year := "1954"]
new_plaques[X == "808", Year := "1914"]
new_plaques[X == "814", Year := "1930-�"]
new_plaques[X =="808", Material := "������"]
new_plaques[X == "194", Year := "1856"]

new_plaques[, Pl_Period := "unknown period"]
new_plaques[, Pl_y := as.numeric(stri_extract_last_regex(Year, "\\d{4}"))]

for (row in 1:nrow(new_plaques)) {
  if (is.na(new_plaques[row, 'Pl_y'])) {
    new_plaques[row, 'Pl_Period'] <- "Unknown period"
  }
  else if (new_plaques[row, 'Pl_y'] < 1918) {
    new_plaques[row, 'Pl_Period'] <- "1. Empire"
  }
  else if (new_plaques[row, 'Pl_y'] >= 1918 && new_plaques[row, 'Pl_y'] < 1928){
    new_plaques[row, 'Pl_Period'] <- "2. Early soviet"
  }
  else if (new_plaques[row, 'Pl_y'] >= 1928 && new_plaques[row, 'Pl_y'] <= 1953){
    new_plaques[row, 'Pl_Period'] <- "3. Stalin time"
  }
  else if (new_plaques[row, 'Pl_y'] > 1953 && new_plaques[row, 'Pl_y'] <= 1964) {
    new_plaques[row, 'Pl_Period'] <- "4. Ottepel"
  }
  else if (new_plaques[row, 'Pl_y'] > 1964 && new_plaques[row, 'Pl_y'] < 1985) {
    new_plaques[row, 'Pl_Period'] <- "5. Zastoi"
  }
  else if (new_plaques[row, 'Pl_y'] >= 1985 && new_plaques[row, 'Pl_y'] <= 1991) {
    new_plaques[row, 'Pl_Period'] <- "6. Perestroika"
  }
  else if (new_plaques[row, 'Pl_y'] > 1991) {
    new_plaques[row, 'Pl_Period'] <- "7. Federation"
  }
}

new_plaques[, Person := as.factor(Person)]
new_plaques[, Name := as.factor(Name)]
new_plaques[, Material := as.factor(Material)]
new_plaques[, what_did := as.factor(what_did)]
new_plaques[, what_did := droplevels(what_did)]
new_plaques[, Gender := as.factor(Gender)]
new_plaques[, Pl_Period := as.factor(Pl_Period)]
new_plaques[, Pl_Period := droplevels(Pl_Period)]
new_plaques[-c(906), ]

#number+material of plaques in periods
ggplot(new_plaques, aes(x = Pl_Period,
                        fill = Material,
                        las = 3)) +
  geom_bar() +
  theme_classic()+
  geom_bar(color = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Number of plaques by period + Material")

#Lenin stats
lenin_plaques <- new_plaques[new_plaques[, new_plaques$Person == "������ �.�."]]
#Lenin in periods
ggplot(lenin_plaques, aes(x = Pl_Period,
                          fill = Material)) +
  theme_classic()+
  geom_bar(color = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Number pf plaques by period: Lenin")

#Lenin in years+is sculptor
ggplot(lenin_plaques, aes(x = Pl_y,
                          fill = Is_sculptor)) +
  theme_classic()+
  geom_bar(colour="white") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(title = "Number pf plaques by year: Lenin")
 
#connections
ggplot(new_plaques, aes(x = "", fill = what_did)) +
  geom_bar(color = "white") +
  coord_polar("y", start=1)+
  labs(title = "Connections")+
  theme_void() 

pie(new_plaques$what_did, names2)

# new_plaques[, what_did1 := NA]
# new_plaques[, what_did1 := as.character(what_did1)]

table(new_plaques$what_did)

#connection between death date and plaque date in death related plaques
ggplot(filter(new_plaques, what_did == "����"| what_did == "���"), aes(x = DeathDate, y = Pl_y)) + 
  geom_point(aes(color = what_did)) + 
  labs(title = 'Death Date vs Plaque')

#is_fancy_on map
new_plaques[, Is_fancy := FALSE]

for (row in 1:nrow(new_plaques)) {
  if (is.na(new_plaques[row, 'Material'])) {
    new_plaques[row, 'Is_fancy'] = FALSE
  }
  else if (new_plaques[row, 'Material'] == "������" || new_plaques[row, 'Is_sculptor'] == TRUE) {
    new_plaques[row, 'Is_fancy'] = TRUE
  }
  else {
    new_plaques[row, 'Is_fancy'] = FALSE
  }
}


map <- GetMap(center = c(lat = 59.931770, lon = 30.308300), size = c(640, 640), zoom = 12)

register_google(key = "AIzaSyApbGcAM8vQ13zp-sZUm0skQjRBWwCdVe8")
map <- get_map(location = c(lon = 30.308300, lat = 59.931770), size = c(640, 640), zoom = 12)

ggmap(map) + geom_point(aes(x = pl_longitude, y = pl_latitude, 
                            color = Is_fancy), size = 1, data = new_plaques) +
  labs(title = 'The location of plaques')


str(new_plaques)

new_plaques[, Pl_period := NULL]
new_plaques[, V1 := NULL]
new_plaques[-c(906), ]

people_stat <- as.data.frame(head(setorderv(new_plaques[, .N, by = "Person"], "N", -1)[], 10))

