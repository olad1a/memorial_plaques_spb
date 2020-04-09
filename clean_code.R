#packages
packages <- c("data.table", "stringi", "stringr", "rvest", "httr", "xml2", "readr", "tidyverse", "ggmap", 
              "lubridate", "anytime", "ggplot2")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

capture.output(lapply(packages, library, character.only = T), file='NULL')

##to change WD to project folder
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

#write.csv(new_df, file = 'new_df', row.names = FALSE, fileEncoding = "UTF-8")

#separating

#description
new_df <- df_plaques %>% separate(DESCRIPTION, c("Trash", "Description", "Other"), sep = "([\\«\\»])") %>%
  select (-Trash)

#person
new_df <- new_df %>% separate(TITLE, c("Person", "z"), sep = ",") %>% select(-z)
new_df <- new_df %>% separate(Person, c("Person", "z"), sep = "\\[") %>% select(-z)

#address
new_df <- new_df %>% separate(Other, c("Other", "Address"), sep = "Адреса")
new_df <- new_df %>% separate(Address, c("Address", "q"), sep = "Предметный") %>% select(-q) 

#creators
new_df <- new_df %>% separate(Other, c("Other", "Architect"), sep = "Арх.")

#year
new_df <- new_df %>% separate(Other, c("Year", "Other"), sep = ". ", extra = "merge")

#storing sculptor and material in a variable that I'll need later
plaques <- new_df %>% unite("Other", Other:Architect, remove = TRUE) %>%
  select(-c("ADDRESS", "ARCHITECT"))

#NA replacement
ind <- which(is.na(new_df$Address))
setDT(plaques)

plaques[ID == "2805524710", Address := "кан. Грибоедова наб./Санкт-Петербург, д. 2, лит. А"]
plaques[ID == "2805531115", Address := "21-я линия В.О./Санкт-Петербург, д. 8, лит. А"]
plaques[ID == "2805530415", Address := "Лоцманская ул./Санкт-Петербург, д. 19"]
plaques[ID == "2805531580", Address := "Средний пр./Санкт-Петербург, д. 82"]
plaques[ID == "2805550894", Address := "Гороховая ул./Санкт-Петербург, д. 56"]
plaques[ID == "2805551543", Address := "Исаакиевская пл./Санкт-Петербург, д. 9"]
plaques[ID == "2805524985", Address := "Невский пр./Санкт-Петербург, д. 56"]
plaques[ID == "2805551911", Address := "Б. Сампсониевский пр./Санкт-Петербург, д. 60"]
plaques[ID == "2805545071", Address := "Фрунзе ул./Санкт-Петербург, д. 18"]
plaques[ID == "2805527011", Address := "Болотная ул./Санкт-Петербург, д. 13"]
plaques[ID == "2805526998", Address := "Болотная ул./Санкт-Петербург, д. 13"]
plaques[ID == "2805527030", Address := "Заставская ул./Санкт-Петербург, д. 33"]
plaques[ID == "2805545161", Address := "Лиговский пр./Санкт-Петербург, д. 281"]
plaques[ID == "2805527143", Address := "Стачек пр./Санкт-Петербург, д. 47"]
plaques[ID == "2805524999", Address := "Рижский пр./Санкт-Петербург, д. 7"]
plaques[ID == "2805545313", Address := "Комарово пос. ж/д ст"]
plaques[ID == "2805527261", Address := "Санкт-Петербург, Фокина ул., 1."]
plaques[ID == "2805528644", Address := "Санкт-Петербург, ж/д ст. Удельная"]
plaques[ID == "2805528218", Address := "Белоостров пос. ж/д ст"]
plaques[ID == "2805527421", Address := "Обводного канала наб., 223–225."]
plaques[ID == "2805527594", Address := "Сестрорецк, пос. Разлив, ул. Емельянова, д. 3."]
plaques[ID == "2805528048", Address := "Новоалександровская ул./Санкт-Петербург, д. 23"]
plaques[ID == "2805528068", Address := "Санкт–Петербург, Полюстровский пр.59"]
plaques[ID == "2805528076", Address := "Санкт–Петербург, Полюстровский пр.59"]
plaques[ID == "2805528192", Address := "Сестрорецк, пос. Тарховка, Дорога к Шалашу Ленина, д. 3, литер А"]
plaques[ID == "2805545966", Address := "Фучика ул./Санкт-Петербург, д. 15"]
plaques[ID == "2805525156", Address := "Никольская пл./Санкт-Петербург 1/3"]
plaques[ID == "2805525759", Address := "Милионная ул./Санкт-Петербург, д. 33"]
plaques[ID == "2805547031", Address := "Фрунзе ул./Санкт-Петербург, д. 18"]
plaques[ID == "2805553878", Address := "Расстанная ул., 30"]
plaques[ID == "2805547577", Address := "В.О., 21- линия, 8а"]
plaques[ID == "2805555122", Address := "пр. Добролюбова/Санкт-Петербург, д. 18"]
plaques[ID == "2805555302", Address := "Невский пр./Санкт-Петербург, д. 85"]
plaques[ID == "2805559085", Address := "Никольская пл./Санкт-Петербург 1/3"]
plaques[ID == "2805548815", Address := "21-я линия В.О./Санкт-Петербург, д. 8, лит. А"]
plaques[ID == "2805548978", Address := "7-я линия В.О./Санкт-Петербург, д. 2"]
plaques[ID == "2805549217", Address := "Бабушкина ул./Санкт-Петербург, д. 123"]

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
plaques <- cbind(plaques, pl_latitude)
plaques <- cbind(plaques, pl_longitude)

#creating material and sculptor column
plaques[, Material := NA]
plaques[, Is_sculptor := NA]
plaques[, Material := as.character(Material)]

for (row in 1:nrow(plaques)) {
  if (grepl("мрамор", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "мрамор"
  }
  else if (grepl("гранит", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "гранит"
  }
  else if (grepl("бронза", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "бронза"
  }
  else if (grepl("металл", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "металл"
  }
  else if(grepl("латунь", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Material'] = "латунь"
  }
  if (grepl("скульптор", plaques[row, 'Other'], ignore.case = T)) {
    plaques[row, 'Is_sculptor'] = T}
    else {
      plaques[row, 'Is_sculptor'] = F}
}

#removing extra colums
plaques[, Other := NULL]

#creating dataset
id <- 2805524710
person <- "Александр II"
description <- "some text"
year <- "1907"
address <- "кан. Грибоедова наб./Санкт-Петербург, д. 2, лит. А"
alex_pl_longitude <- "30.3289"
alex_pl_latitude <- "59.9401"
material <- "гранит"
is_sculptor <- "FALSE"

for (i in 1:18) {
  id <- c(id, paste0("2805524710", i))
  person <- c(person, "Александр II")
  description <- "some text"
  year <- c(year, "1907")
  address <- c(address, "кан. Грибоедова наб./Санкт-Петербург, д. 2, лит. А")
  alex_pl_longitude <- c(alex_pl_longitude, 30.3289)
  alex_pl_latitude <- c(alex_pl_latitude, 59.9401)
  material <- c(material, "гранит")
  is_sculptor <- c(is_sculptor, FALSE)
}

#creating dataframe
alex_df <- data.table(
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
alex_df[ID == "2805524710", Description := "17 апреля 1818 года. 
        Рождение Великого Князя Александра Николаевича в Московском Кремле. 
        “Да встретит он обильный честью век, Да славного участник славный будет, 
        Да чреде высокой не забудет, Святейшего из званий человек!” Жуковский. 1818 г."]
alex_df[ID == "28055247101", Description := "12 декабря 1852 года. Провозглашение Великого Князя АЛЕКСАНДРА НИКОЛАЕВИЧА 
        Наследником Престола. 16 апреля 1841 года. Бракосочетание Наследника Цесаревича и Великого Князя АЛЕКСАНДРА 
        НИКОЛАЕВИЧА с Великою Княжною МАРИЕЮ АЛЕКСАНДРОВНОЮ, Принцессою Гессен-Дармштадтскою"]
alex_df[ID == "28055247102", Description := "19 февраля 1855 года. Восшествие на Престол Государя Императора Александра II.
        “... Вступая на прародительский НАШ Престол Российский Империи и неразделимых с нею Царства Польского 
        и Великого Княжества Финляндского перед лицом невидимо соприсутствующего НАМ БОГА, приемлем священный обет иметь 
        всегда единою целью благоденствие Отечества НАШЕГО. Да руководимые, покровительствуемые призвавшим НАС 
        к сему Великому служению Провидением утвердим Россию на высшей степени могущества и славы, 
        да исполняются через НАС постоянные желания и виды Августейших НАШИХ предшественников ПЕТРА, ЕКАТЕРИНЫ, 
        АЛЕКСАНДРА Благословенного и Незабвенного НАШЕГО Родителя” Александр"]
alex_df[ID == "28055247103", Description := "26 августа 1856 года. Священное коронование и Миропомазание 
        ГОСУДАРЯ ИМПЕРАТОРА АЛЕКСАНДРА НИКОЛАЕВИЧА и ГОСУДАРЫНИ ИМПЕРАТРИЦЫ МАРИИ АЛЕКСАНДРОВНЫ в Москве. 
        “В сей торжественный день, когда, испросив благословение ВСЕВЫШНЕГО, МЫ возложили на СЕБЯ венец 
        НАШИХ Предков, первою НАШЕЮ мыслию было, как всегда, благоденствие любезной НАМ России. Повторяя 
        при Священном обряде Коронования обет, произнесенный НАМИ в самый час вступления НАШЕГО на 
        Прародительский Престол, иметь постоянною, единою целию трудов и попечений НАШИХ утверждение, 
        возвышение сего благоденствия в настоящем и будущем времени. МЫ не могли, с тем вместе, не обратиться 
        и к воспоминанию о событиях недавно минувших лет, ознаменованных тягостными испытаниями, но и примерами 
        высокой доблести и новыми доказательствами беспредельной, нелицемерной преданности верных подданных НАШИХ 
        всех состояний к Престолу и Отечеству...” Александр"]
alex_df[ID == "28055247104", Description := "19 марта 1856 года. Заключение мира, положившего конец Восточной 
        войне 1853–1856 гг. “Упорная, кровопролитная борьба, возмущавшая Европу в течение трех почти лет, 
        прекращается... При помощи Небесного промысла всегда благодеющего России, да утверждается и 
        совершенствуется ее внутреннее благоустройство, правда и милость да царствует в судах ее, да развивается 
        повсюду и с новой силою стремление к просвещению и всякой полезной деятельности, и каждый под сению 
        законов, для всех равно справедливых, всем равно покровительствующих, да наслаждается в мире плодом трудов
        невинных. Наконец, и сие есть первое живейшее желание наше, свет спасительной веры, озаряя умы, укрепляя 
        сердца, да сохраняет и улучшает более и более общественную нравственность, сей вернейший залог порядка и 
        счастья...” Александр"]
alex_df[ID == "28055247105", Description := "1858 и 1860 гг. Присоединение к России Амурского и Уссурийского края. 
        Айгунский договор 16 мая 1858 года. Пекинский договор 2 ноября 1860 года"]
alex_df[ID == "28055247106", Description := "1859 и 1864 гг. Покорение Кавказа. Взятие Гуниба и пленение Шамиля 
        25 августа 1859 года. Окончание Кавказской войны 25 мая 1864 года"]
alex_df[ID == "28055247107", Description := "19 февраля 1861 года. Освобождение крестьян от крепостной зависимости. 
        “Осени себя крестным знамением, православный народ, и призови с НАМИ БОЖИЕ благословение на твой свободный труд, 
        залог твоего домашнего благополучия и блага общественного...” Александр»"]
alex_df[ID == "28055247108", Description := "22 мая 1862 года. Положение о Государственной росписи и финансовых 
сметах и об обнародовании их во всеобщее сведение.
1862–1866 гг. Преобразование Государственного Контроля. Развитие сети железных дорог и телеграфов"]
alex_df[ID == "28055247109", Description := "17 апреля 1863 года. Ограничение телесных наказаний"]
alex_df[ID == "280552471010", Description := "20 ноября 1864 года. Судебные Уставы. “Рассмотрев сии проекты, 
        МЫ находим, что они вполне соответствуют желанию НАШЕМУ водворить в России суд скорый правый, милостивый 
        и равный для всех подданных НАШИХ, возвысить судебную власть, дать ей надлежащую самостоятельность и вообще 
        утвердить в народе НАШЕМ то уважение к закону, без коего невозможно общественное благосостояние и которое 
        должно быть постоянным руководителем действий всех и каждого от высшего до низшего...” Александр"]
alex_df[ID == "280552471011", Description := "19 февраля 1864 года. Устройство быта крестьян Царства Польского. 
        “Ныне совершилось ровно три года с тех пор, как в день 19 февраля 1861 года МЫ издали Манифест и
        положение об устройстве крестьян в России. И в Царстве Польском настоящий день МЫ знаменуем
        исполнением священного завета Родителя НАШЕГО, НАШИХ Собственных давнишних желаний и упований 
        многочисленного верного НАМ сословия поселян. Да останется сей день вечно памятен и крестьянам 
        Царства, как день вновь возникающего их благосостояния. Да будет сие благосостояние их счастливым 
        предвестником того общего преуспеяния и благоденствия, водворение коего во всех слоях населения 
        Царства составляет предмет НАШЕГО постоянного желания и непобедимой надежды” Александр"]
alex_df[ID == "280552471012", Description := "1 января 1864 года. Положение о земских учреждениях.
16 июня 1870 года. Городовое Положение. 6 апреля 1865 года. Закон о предоставлении печати возможных облегчений"]
alex_df[ID == "280552471013", Description := "Народное образование. “В постоянных заботах МОИХ о благе МОЕГО 
народа Я обращаю особенное МОЕ внимание на дело народного просвещения, видя в нем движущую силу всякого успеха 
и утверждение тех нравственных основ, на которых зиждутся государства” Александр. Положение о начальных народных 
        училищах 1864 г. Городские училища и Учительские институты 1872 г. Уставы Гимназий и Прогимназий 1864–1871 гг. 
        Уставы Реальных училищ 1872 г. Университетский устав 1863 г. Попечение о развитии женского образования"]
alex_df[ID == "280552471014", Description := "1 января 1874 года. Всеобщая воинская повинность. 
        “...Сила Государства не в одной численности войск, но преимущественно в нравственных и умственных 
        его качествах, достигающих высшего развития лишь тогда, когда дело защиты отечества становится 
        общим делом народа, когда все без различия званий и состояний соединяются на это святое дело... 
        Утвердив... Устав о воинской повинности и призывая подданных НАШИХ именем дорогой всем НАМ отчизны 
        к ревностному исполнению возлагаемых на них обязанностей, МЫ не имеем намерений отступать от начал, 
        которым неуклонно следовали во все НАШЕ Царствование. МЫ не ищем, как не искали до сих пор блеска 
        военной славы и лучшим жребием, ниспосланным НАМ от БОГА, почитаем вести Россию к величию путем мирного 
        преуспеяния и всестороннего внутреннего развития...” Александр"]
alex_df[ID == "280552471015", Description := "19 октября 1870 года. Возвращение Россиею своих Державных прав на Черном море"]
alex_df[ID == "280552471016", Description := "Завоевание Средней Азии 1860–1881 гг. Взятие штурмом города Ташкента. Победа над 
        Бухарскими войсками у Ирджара. Взятие штурмом Кокандского города Ходжента. Взятие Бухарских крепостей Ура-Тюбе 
        и Джизак. Бой у Яны-Кургана. Штурм Самаркандских высот и занятие города. Бой на Зарабулавских высотах и занятие 
        их. Занятие Хивы, столицы Ханства. Взятие Кокандской крепости Махрам. Вторичное взятие штурмом города Андижан. 
        Присоединение к владениям России Кокандского Ханства. Взятие штурмом крепости Геок-Тепе"]
alex_df[ID == "280552471017", Description := "19 февраля 1878 года. Заключение мира в С.-Стефано.
        Воссоединение с Россией отторженного участка Бессарабии. Присоединение Карса и Батума. 
        Независимость Румынии, Сербии и Черногории. Освобождение Болгарии"]
alex_df[ID == "280552471018", Description := "Война за освобождение Балканских христиан. 
        12 апреля 1877 – 19 февраля 1878. “Я знаю, что вся Россия вместе со МНОЮ принимает 
        живейшее участие в страданиях наших братий по вере и по происхождению... Вся Россия 
        отзовется на МОЙ призыв, когда я сочту это нужным и честь России того потребует... 
        Да поможет НАМ БОГ исполнить НАШЕ святое призвание” (Из речи, произнесенной ИМПЕРАТОРОМ 
        АЛЕКСАНДРОМ II в Кремлевском Дворце 30 октября 1876 г.) Взятие Ардагана. Переправа через 
        Дунай у Галаца и у Зимницы. Защита Баязета. Взятие Никополя. Захват Балканских походов: 
        Ханикойского и Шипкинского. Взятие Ловчи. Поражение армии Мухтара-Паши на Аладжинских высотах. 
        Горный Дубняк и Телиш. Деве-Бойну. Взятие штурмом Карса. Падение Плевны. Сражение при сс. 
        Трестеника и Мечки. Переход через Балканы: Ташкисена, гор. Бугорок, Комарцы, Петричево Враждебна. 
        Шипка. Шейново. Филиппополь. Занятие Андрианополя. Занятие Эрзерума"]

#fixing data types
alex_df[, ID:=as.character(ID)]
alex_df[, Person:=as.character(Person)]
alex_df[, Description:=as.character(Description)]
alex_df[, Year:=as.character(Year)]
alex_df[, Address:= as.character(Address)]
alex_df[, pl_latitude:=as.character(pl_latitude)]
alex_df[, pl_longitude:=as.character(pl_longitude)]

#binding datasets
new_plaques <- rbind(plaques, alex_df, use.names=TRUE, stringsAsFactors=FALSE, sort=TRUE, fill=TRUE)
new_plaques <- new_plaques[-6,] #dirty row with Alexander II

#more cleaning
str(new_plaques)

new_plaques[, Person := gsub("\\. ", ".", Person)]
new_plaques[, Person := as.factor(Person)]

new_plaques[, x := NULL]
new_plaques <- plaques[-907,]

#connections
new_plaques[, what_did := NA]
new_plaques[, what_did := as.character(what_did)]

for (row in 1:nrow(new_plaques)) {
  if (grepl("жил\\s+и\\s+работал", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "жил и работал"
  }
  else if (grepl("учился|учились|училась|обучался|воспитанник|воспитывалась|воспитывался|обучалась|окончил", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "учился"
  }
  else if (grepl("автору\\s+проекта|создан|сооружен|основал|основат|основана|создан", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "создал"
  }
  else if (grepl("назван|носит\\s+имя|названа\\s+в\\s+честь|присвоено\\s+имя|имени|присвоить", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "название в честь"
  }
  else if (grepl("умер|погиб|скончался|оборвалась жизнь|убит|убитого|был казнен", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "умер"
  }
  else if (grepl("погребен|похоронен|покоится", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "похоронен"
  }
  else if (grepl("жил|жила|проживал|квартира", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "жил"
  }
  else if (grepl("работал|работав|трудов|мастерск|лаборатори|кабинет|исследоват|завод
                 преподава|проводил\\s+занятия|проводил\\s+консультации|читал", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "работал"
  }
  else if (grepl("выступал|прочитал|дирижировал|выступил|выступлени", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "выступал"
  }
  else if (grepl("бывал|остановился|останавливался|участвовал\\s+в\\s+совещании|участвовал\\s+в\\s+заседании|
                 встретился|встреча|встречался|состоялась//s+встреча|скрывался|скрывая", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "бывал"
  }
  else if (grepl("руководил|управлял|руководила|председат|директор", new_plaques[row, 'Description'], ignore.case = T)) {
    new_plaques[row, 'what_did'] = "руководил"
  }
  else if (new_plaques[row, 'Person'] == "Александр II") {
    new_plaques[row, 'what_did'] = "убит"
  }
  else
    new_plaques[row, 'what_did'] = "другое"
}

#to check unmatching cases
na_list <- which(is.na(new_plaques$what_did))
na_list
ggplot(new_plaques, aes(x = "", fill = what_did)) +
  geom_bar(color = "white") +
  coord_polar("y", start=1)+
  labs(title = "Connections")+
  theme_void() 

#merging wikidata
spb_persons_wikidata <- fread("spb_persons_wikidata_raw.csv", encoding = "UTF-8")
# Unique entries
spb_persons_wikidata <- unique(spb_persons_wikidata, by = "human")
spb_persons_wikidata[, human := gsub("http://www.wikidata.org/entity/", "", human, fixed = T)]

#filtering dataset
#by bearth/death date
spb_persons <- spb_persons_wikidata[spb_persons_wikidata[,birthdate < "1940-01-01"]]
spb_persons <- spb_persons_wikidata[spb_persons_wikidata[,deathdate < "1999-01-01"]]
#removing non cyrillic names
spb_persons <- spb_persons[spb_persons[, ! grepl("\\d", humanLabel)]]
spb_persons <- spb_persons[spb_persons[, grepl("[йцукенгшщзхъэждлорпавыфячсмитьбю]", humanLabel, ignore.case = T)]]

#dataset with names from plaques dataset
ppl <- as.data.table(str_split_fixed(new_plaques$Person, "\\s", 2))
ppl <- cbind(ppl, as.data.table(str_split_fixed(ppl$V2, "[.]", 2)))
setnames(ppl, old = "V1", new = "surname")[]
setnames(ppl, old = "V2", new = "initials")[]
setnames(ppl, old = "V1", new = "init_1")[]
setnames(ppl, old = "V2", new = "init_2")[]

#dataset with names from wikidata
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

to add info from wiki df to plaques df
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

new_plaques <- fread('pplplaques.csv', encoding = "UTF-8")


###Cleaning resulting data
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

#converting dates to POSIX 
new_plaques[, BirthDate := anytime::anytime(BirthDate)]
new_plaques[, DeathDate := anytime::anytime(DeathDate)]
#Y-m-d
new_plaques[, BirthDate := anytime::anydate(BirthDate)]
new_plaques[, DeathDate := anytime::anydate(DeathDate)]


## How many people didn't live, work, died or were born in SPb based on Wikidata
no_wiki <- new_plaques[new_plaques[, is.na(new_plaques$Name)]]
no_wiki <- no_wiki[!duplicated(no_wiki[,Person]), ]
nrow(no_wiki)

##Analysing plaquing patterns based on time period
new_plaques[, Pl_Period := "unknown period"]

#clean the Year column more
new_plaques[X == "808", Year := "1914"]
new_plaques[X == "145", Year := "1970"]
new_plaques[X == "513", Year := "1915"]
new_plaques[X == "28", Year := "1975"] 
new_plaques[X == "62", Year := "1998"] 
new_plaques[X == "508", Year := "1954"]
new_plaques[X == "808", Year := "1914"]
new_plaques[X == "814", Year := "1930-е"]
new_plaques[X =="808", Material := "мрамор"]
new_plaques[X == "194", Year := "1856"]
```

```{r}
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

#fixing types
new_plaques[, Person := as.factor(Person)]
new_plaques[, Name := as.factor(Name)]
new_plaques[, Material := as.factor(Material)]
new_plaques[, what_did := as.factor(what_did)]
new_plaques[, what_did := droplevels(what_did)]
new_plaques[, Gender := as.factor(Gender)]
new_plaques[, Pl_Period := as.factor(Pl_Period)]
new_plaques[, Pl_Period := droplevels(Pl_Period)]
new_plaques <- new_plaques[-c(906), ]

# Analysing resulting data
ggplot(new_plaques, aes(x = Pl_Period,
                        fill = Material,
                        las = 3)) +
  geom_bar() +
  theme_classic()+
  geom_bar(color = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Number of plaques by period + Material")

##Lenin stats
lenin_plaques <- new_plaques[new_plaques[, new_plaques$Person == "Ленину В.И."]]
ggplot(lenin_plaques, aes(x = Pl_Period,
                          fill = Material)) +
  theme_classic()+
  geom_bar(color = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Number pf plaques by period: Lenin")

ggplot(lenin_plaques, aes(x = Pl_y,
                          fill = Is_sculptor)) +
  theme_classic()+
  geom_bar(colour="white") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(title = "Number pf plaques by year: Lenin")


## Is there connectionin general between death date and plaque date in death related plaques? 
#
ggplot(filter(new_plaques, what_did == "умер"| what_did == "жил"), aes(x = DeathDate, y = Pl_y)) + 
  geom_point(aes(color = what_did)) + 
  labs(title = 'Death Date vs Plaque')

##  Let's look at their locations.
register_google(key = "AIzaSyApbGcAM8vQ13zp-sZUm0skQjRBWwCdVe8")
map <- get_map(location = c(lon = 30.308300, lat = 59.931770), size = c(640, 640), zoom = 12)

ggmap(map) + geom_point(aes(x = pl_longitude, y = pl_latitude), size = 1, data = new_plaques) +
  labs(title = 'The location of plaques')