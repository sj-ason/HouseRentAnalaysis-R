# JASON YITRO SETIADI
# TP062295

house_rent <- read.csv("/home/jasonyitro/ExpanDrive/OneDrive Business/2 Sem 1 1/PFDA/Assignment/House_Rent_Dataset.csv", stringsAsFactors = TRUE)
View(house_rent)

str(house_rent)
names(house_rent)
summary(house_rent)

library(ggplot2)
library(scales)
library(stringr)
library(dplyr)

### DATA TRANSFORMATION AND MANIPULATION
# RENAME
house_rent <- house_rent %>%
  rename(Tenant = Tenant.Preferred)
# SUBSET
bach_bhk <- subset(house_rent, Tenant == "Bachelors")
tenant_family <- subset(house_rent, Tenant == "Family")
tenant_bachelors <- subset(house_rent, Tenant == "Bachelors")
# FILTER
mum_house <-  filter(house_rent, City == "Mumbai")
# SAMPLE
tenant_family_1 <- sample_frac(tenant_family, .25)
area_local <- sample_n(house_rent, 300)
# REPLACE
house_rent$FloorNum = factor(str_extract(house_rent$Floor, "(\\w+)"))
house_rent$TotNumFloors = factor(str_extract(house_rent$Floor, "\\w+$"))

fl_str = c("Ground" = "0", "Upper" = "-1", "Lower" = "-2")
house_rent$FloorNum_Rev <- str_replace_all(house_rent$FloorNum, fl_str)

tfl_str = c("Ground" = "0")
house_rent$TotNumFloors <- str_replace_all(house_rent$TotNumFloors, tfl_str)
# FORMAT
house_rent$Month = format(as.Date(house_rent$Posted.On, format = "%m/%d/%Y"), "%m")

# Question 1: Why Tenant Bachelors Has Higher Rent Houses?
## Analysis 1.1: Find The Relationship Between Rent And Size
ggplot(house_rent, aes(x = Rent, y = Size)) +
  geom_point(color = "red") +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma) +
  geom_smooth()

## analysis 1.2: find the relationship between Tenant and rent
ggplot(house_rent, aes(x = Tenant, y = Rent)) +
  geom_bar(aes(fill = Tenant), stat = "identity") +
  scale_y_continuous(labels = comma)

## Analysis 1.3: Find The Relationship Between Tenant Bachelors And Size
ggplot(house_rent, aes(x = Tenant, y = Size)) +
  geom_boxplot(aes(fill = Tenant)) +
  scale_y_continuous(labels = comma)

## Analysis 1.4: Find The Relationship Between Tenant Bachelors And City
ggplot(house_rent, aes(x = Tenant)) +
  geom_bar(aes(fill = City)) +
  geom_text(aes(label = ..count..), stat = "count") +
  xlab("Tenant") + 
  ylab("Count")

ggplot(house_rent, aes(x = City, y = Tenant)) +
  geom_bar(aes(fill = Tenant == "Bachelors"), stat = "identity", position = "dodge")

## Analysis 1.5 Relationship Between Tenant Bachelors With BHK
ggplot(bach_bhk, aes(x = factor(BHK))) +
  geom_bar(aes(fill = Tenant)) +
  ylab("Count") +
  xlab("BHK Numbers")

## Analysis 1.6 Relationship Between BHK And Rent
ggplot(bach_bhk, aes(x = factor(BHK), y = Rent)) +
  geom_bar(aes(fill = BHK), fun = "mean", stat = "summary") +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
  scale_y_continuous(labels = comma) +
  xlab("BHK Numbers") 

ggplot(bach_bhk, aes(x = factor(BHK), y = Rent)) +
  stat_sum(alpha = 0.25)+
  xlab("BHK Numbers") +
  scale_y_continuous(labels = comma)

ggplot(bach_bhk, aes(x = factor(BHK), y = Rent)) +
  geom_jitter()+
  xlab("BHK Numbers") +
  scale_y_continuous(labels = comma)

## Analysis 1.7 Relationship Between FloorNum_Rev and Bachelors
ggplot(bach_bhk, aes(x = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = Tenant)) +
  ylab("Count") +
  xlab("Floor Numbers")

ggplot(bach_bhk, aes(x = factor(FloorNum_Rev, levels = -2:76), y = Rent)) +
  geom_bar(aes(fill = factor(FloorNum_Rev, levels = -2:76)), stat = "identity") +
  scale_y_continuous(labels = comma) +
  xlab("Floor Numbers") +
  scale_fill_manual("Floor Numbers", values = factor(bach_bhk$FloorNum_Rev, levels = -2:76))

# Question 2: Why Mumbai Is The Most Expensive City And What Are The Connections To The Houses?
## Analysis 2.1: Which House Has the Most Expensive Rent and What is Its Characteristics?
mx_hr <- max(house_rent$Rent)
house_rent[house_rent$Rent == mx_hr,] # Turns out Bangalore

## Analysis 2.2: Relationship Between City and Rent
ggplot(house_rent, aes(x = City, y = Rent)) +
  geom_boxplot(aes(fill = City)) +
  scale_y_continuous(labels = comma)

## Analysis 2.3: Why Is It Expensive? Is It Because The Area?
ggplot(house_rent, aes(x = City, y = Size)) +
  geom_boxplot(fill = c("lightblue", "brown", "black", "darkgreen", "red", "purple")) +
  xlab("City")

## Analysis 2.4: What Furnishing Status Does It Have in Mumbai? Should It Be Fully Furnished, Semi Furnished, Or Unfurnished?
ggplot(mum_house, aes(x = Furnishing.Status)) +
  geom_bar(fill = c('red', 'darkblue', 'purple')) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.4, color = "white") +
  xlab("Furnishing Status") + 
  ylab("Count")

ggplot(mum_house, aes(x = Furnishing.Status, y = Rent)) +
  geom_bar(aes(fill = City), fun = "mean", stat = "summary") +
  scale_y_continuous(labels = comma)

ggplot(mum_house, aes(x = Furnishing.Status, y = Rent)) +
  geom_bar(aes(fill = City), stat = "identity") +
  scale_y_continuous(labels = comma) +
  xlab("Furnishing Status")

## Analysis 2.5: Relationship Between Mumbai City and BHK
ggplot(house_rent, aes(factor(City), fill=factor(BHK))) +
  geom_bar(position = 'stack') +
  coord_flip() +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  ylab("Count") +
  xlab("City") +
  scale_fill_manual("BHK Numbers", values = c("#f8766d", "#b79f00", "#00ba38", "#00bfc4", "#619cff", "#f564e3"))

ggplot(house_rent, aes(x = factor(BHK), y = Rent)) +
  geom_bar(aes(fill = factor(BHK)), stat = "identity") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual("BHK Numbers", values = c("#f8766d", "#b79f00", "#00ba38", "#00bfc4", "#619cff", "#f564e3"))

## Analysis 2.6: Relationship Between Mumbai City and Bathroom
ggplot(house_rent, aes(factor(City), fill=factor(Bathroom))) +
  geom_bar(position = 'stack') +
  coord_flip() +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  ylab("Count") +
  xlab("Bathroom") +
  scale_fill_manual("Bathroom", values = c("#f8766d", "#b79f00", "#7cae00", "#00be67", "#00bfc4", "#00a9ff", "#c77cff", "#ff61cc"))

ggplot(house_rent, aes(x = factor(Bathroom), y = Rent)) +
  geom_bar(aes(fill = factor(Bathroom)), stat = "identity") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual("Bathroom", values = c("#f8766d", "#b79f00", "#7cae00", "#00be67", "#00bfc4", "#00a9ff", "#c77cff", "#ff61cc")) +
  xlab("Bathroom")
  
#Question 3: Why Floor Number Related With To Rent And Size Of A House?
## Analysis 3.1: Relationship Floor Number With Size
max(house_rent$Size)

ggplot(house_rent, aes(x = factor(FloorNum_Rev, levels = -2:76), y = Size)) +
  geom_point(color = "#008000") +
  scale_y_continuous(labels = comma) +
  xlab("Floor Numbers")

## Analysis 3.2 Relationship Between Floor Number With City
ggplot(house_rent, aes(x = City, y = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = factor(FloorNum_Rev, levels = -2:76)), stat = "identity", position = "dodge") +
  ylab("Floor Numbers")

## Analysis 3.3 Relationship Between Floor Number With Rent
ggplot(house_rent, aes(x = factor(FloorNum_Rev, levels = -2:76), y = Rent)) +
  geom_point(color = "#508019") +
  scale_y_continuous(labels = comma) +
  xlab("Floor Numbers")

## Analysis 3.4 Relationship Between Area.Type And Floor
ggplot(house_rent, aes(x = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = Area.Type)) +
  geom_text(aes(label = ..count..), size = 3, stat = "count") +
  xlab("Floor Number") + 
  ylab("Count")
  
## Analysis 3.5 Relationship Between Area Type And Rent
ggplot(house_rent, aes(x = Area.Type, y = Rent)) +
  geom_boxplot(aes(fill = Area.Type)) +
  scale_y_continuous(labels = comma) +
  xlab("Area Type")

## Analysis 3.6 Relationship Between Area Type And Size 
max(house_rent$Size)
ggplot(house_rent, aes(x = Area.Type, y = Size)) +
  geom_boxplot(aes(fill = Area.Type)) +
  scale_y_continuous(labels = comma) +
  xlab("Area Type")

## Analysis 3.7 Relationship Between Area Type And City
ggplot(house_rent, aes(x = City)) +
  geom_bar(aes(fill = Area.Type, color = Area.Type)) +
  xlab("City") + 
  ylab("Count") +
  geom_text(aes(label = ..count..), stat = "count")

# Question 4: Why The Amount Of BHK And Bathroom Increase With Rent? 
## Analysis 4.1 Relationship Between BHK and Rent
ggplot(house_rent, aes(x = BHK, y = Rent)) +
  geom_point(aes(color = Rent)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_colour_continuous(labels = comma)

## Analysis 4.2 Relationship Between BHK And City
min(house_rent$BHK)
ggplot(house_rent, aes(x = City, y = factor(BHK))) +
  geom_bar(aes(fill = factor(BHK)), stat = "identity", position = "dodge") +
  ylab("BHK Number")

## Analysis 4.3 Relationship Between Bathroom And Rent
max(house_rent$Bathroom)
ggplot(house_rent, aes(x = Bathroom, y = Rent)) +
  geom_point(aes(color = Rent)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_colour_continuous(labels = comma)

house_rent[house_rent$Bathroom == max(house_rent$Bathroom),]

## Analysis 4.4 Relationship Between Bathroom And City
ggplot(house_rent, aes(x = City, y = Bathroom)) +
  geom_boxplot(aes(fill = City), color = "blue")

# Question 5: Does Family Prefer More BHK? Bathroom As Well?
## Analysis 5.1 Tenant Pie Chart
bach <- nrow(house_rent[house_rent$Tenant == "Bachelors",])
bach_fam <- nrow(house_rent[house_rent$Tenant == "Bachelors/Family",])
fam <- nrow(house_rent[house_rent$Tenant == "Family",])
bach
bach_fam
fam

TenantPreferred <- c(bach, bach_fam, fam)
NamesTenantPreferred <- c("Bachelors: 830", "Bachelors/Family: 3444", "Family: 472")

pie(TenantPreferred, NamesTenantPreferred, main = "TENANT_PREFERRED", clockwise = TRUE)

## Analysis 5.2 Relationship Between Tenant Family And BHK
max(house_rent$BHK)
ggplot(house_rent, aes(x = Tenant, y = BHK)) +
  geom_boxplot(aes(fill = Tenant))

ggplot(tenant_family, aes(x = factor(BHK))) +
  geom_histogram(aes(fill = factor(BHK)), stat = "count", binwidth = 0.1) +
  geom_text(aes(label = ..count..), stat = "count") + 
  ylab("Count") +
  xlab("BHK Number")

ggplot(tenant_bachelors, aes(x = factor(BHK))) +
  geom_histogram(aes(fill = factor(BHK)), stat = "count") +
  geom_text(aes(label = ..count..), stat = "count") + 
  ylab("Count") +
  xlab("BHK Number")

## Analysis 5.3 Relationship Between Family Tenant And Size
max(house_rent$Size)

ggplot(house_rent, aes(x = Tenant, y = Size)) +
  geom_bar(aes(fill = Tenant), stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual("Tenant", values = c("#f8766d", "#f8766d", "lightblue"),
                    labels = c("Bachelors", "Bachelors/Family", "Family"))

ggplot(house_rent, aes(x = Tenant, y = Size)) +
  geom_bar(fill = "#fdc9cd", fun = "mean", stat = "summary")

ggplot(tenant_family_1, aes(y = factor(Size))) +
  geom_bar(aes(fill = factor(Size))) +
  xlab("Count") +
  ylab("Size")

## Analysis 5.4 Relationship Between Family Tenant and Bathroom
ggplot(house_rent, aes(x = Tenant, y = Bathroom)) +
  geom_bar(aes(fill = Tenant == "Family"), fun = "mean", stat = "summary", position = "dodge") +
  scale_y_continuous(labels = comma)

## Analysis 5.5 Relationship Between Family Tenant and Rent
ggplot(tenant_family, aes(x = Tenant, y = Rent)) +
  geom_violin(fill = "#b5bbe3")

## Analysis 5.6 Relationship Between Family Rent and BHK/Bathroom
ggplot(house_rent, aes(x = factor(BHK), y = Rent)) +
  geom_bar(aes(fill = factor(BHK)), stat = "identity") +
  xlab("BHK Numbers") +
  scale_y_continuous(labels = comma)

ggplot(house_rent, aes(x = factor(Bathroom), y = Rent)) +
  geom_bar(aes(fill = factor(Bathroom)), stat = "identity") +
  xlab("Bathroom") +
  scale_y_continuous(labels = comma)

# Question 6: Why Point Of Contact In More Expensive City Is Through Contact Agent?
## Analysis 6.1 Relationship Between Point.of.Contact and City
ggplot(house_rent, aes(x = City, y = Point.of.Contact)) +
  geom_bar(aes(fill = Point.of.Contact), stat = "identity", position = "dodge") +
  ylab("Point Of Contact")

ggplot(house_rent, aes(x = City)) +
  geom_bar(aes(fill = Point.of.Contact), position = "dodge") +
  xlab("City") + 
  ylab("Count")

## Analysis 6.2 relationship between point of contact and tenant preferred
ggplot(house_rent, aes(x = Point.of.Contact)) +
  geom_bar(aes(fill = Tenant)) + 
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count")

## Analysis 6.3 Relationship Between Point Of Contact And Furnishing Status.
ggplot(house_rent, aes(x = Point.of.Contact)) +
  geom_bar(aes(fill = Furnishing.Status)) +
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count")

## Analysis 6.4 Relationship Between Point Of Contact And Rent
ggplot(house_rent, aes(x = Point.of.Contact, y = Rent)) +
  geom_boxplot(aes(fill = Point.of.Contact)) +
  scale_y_continuous(labels = comma)

## Analysis 6.5 Relationship Between Point Of Contact And BHK
ggplot(house_rent, aes(x = factor(Point.of.Contact), fill = factor(BHK))) +
  geom_bar() +
  coord_flip() +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  ylab("Count")

# Question 7: Does Area Locality Affects Everything In Certain City?
## Analysis 7.1 Relationship Between Area Locality With Rent
ggplot(area_local, aes(x = Area.Locality, y = Rent)) +
  geom_bar(aes(fill = City), stat = "identity", position ="dodge") +
  scale_y_continuous(labels = comma) +
  xlab("Area Locality")

## Analysis 7.2 Relationship Between Area Locality With Size
max(house_rent$Size)
ggplot(area_local, aes(x = Area.Locality, y = Size)) +
  geom_bar(aes(fill = City), stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  xlab("Area Locality")

## Analysis 7.3 Relationship Between City And Rent
ggplot(area_local, aes(x = City, y = Rent, fill = City)) +
  geom_bar(fun = "mean", stat = "summary") +
  scale_y_continuous(labels = comma)

## Analysis 7.4 Relationship Between Area Locality With Floor Number
ggplot(area_local, aes(x = Area.Locality, y = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = City), stat = "identity", position = "dodge") +
  xlab("Area Locality") +
  ylab("Floor Numbers")

## Analysis 7.5 Relationship Between Area Locality, City, And Area Type
ggplot(area_local, aes(x = Area.Locality, y = City, fill = Area.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("City")

# Question 8: Which City Does Tenant Prefer And What Floor Is The House?
## Analysis 8.1 Relationship Between Tenant And City
ggplot(house_rent, aes(x = Tenant)) +
  geom_bar(aes(fill = City), position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count")

## Analysis 8.2 Relationship Between City and Floor Numbers
ggplot(house_rent, aes(x = City, y = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = factor(FloorNum_Rev, levels = -2:76)), stat = "identity", position = "dodge") +
  ylab("Floor Numbers")

## Analysis 8.3 Relationship Between City and Total Floor Numbers/Height of Building
ggplot(house_rent, aes(x = City, y = factor(TotNumFloors, levels = 0:89))) +
  geom_bar(aes(fill = factor(TotNumFloors, levels = 0:89)), stat = "identity", position = "dodge") +
  ylab("Heights of Building")

ggplot(house_rent, aes(x = City)) +
  geom_bar(aes(fill = factor(TotNumFloors, levels = 0:89))) +
  ylab("Count") +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_manual("Heights of Building", values = factor(house_rent$TotNumFloors, levels = 0:89))

## Analysis 8.4 Relationship Between Tenant And Floor Numbers
# house_rent <- house_rent[,!names(house_rent) %in% c("FloorNum_Rev", "FloorNum")]

ggplot(house_rent, aes(x = Tenant, y = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = factor(FloorNum_Rev, levels = -2:76)), stat = "identity", position = "dodge") +
  ylab("Floor Number") +
  scale_fill_manual("Floor Numbers", values = factor(house_rent$FloorNum_Rev, levels = -2:76))

ggplot(house_rent, aes(x = Tenant)) +
  geom_bar(aes(fill = factor(FloorNum_Rev, levels = -2:76))) +
  ylab("Count") +
  geom_text(aes(label = ..count..), stat = "count") +
  scale_fill_manual("Floor Numbers", values = factor(house_rent$FloorNum_Rev, levels = -2:76))

# QUESITION 9 DOES EXPENSIVE CITY HAVE HIGHER TOTAL OF FLOORS, AND AREA.TYPE
# house_rent <- house_rent[,!names(house_rent) %in% ("TotNumFloors")]

## Analysis 9.1 Relationship Between Floor Number and Height of Building
house_rent$TotNumFloors <- factor(house_rent$TotNumFloors)
ggplot(house_rent, aes(y = factor(TotNumFloors, levels = 0:89), x = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = factor(TotNumFloors, levels = 0:89)), stat = "identity", position = "dodge") +
  ylab("Heights of Building") +
  xlab("Floor Number")

## Analysis 9.2 Relationship Between Height of Building And Area Type
ggplot(house_rent, aes(x = factor(TotNumFloors, levels = 0:89), y = Area.Type)) +
  geom_bar(aes(fill = factor(TotNumFloors, levels = 0:89)), stat = "identity", position = "dodge") +
  ylab("Area Type") +
  xlab("Heights of Building") +
  scale_fill_manual("Months", values = factor(house_rent$TotNumFloors, levels = 0:89))

## Analysis 9.3 Relationship Between Area Type And Tenant
ggplot(house_rent, aes(x = Area.Type)) +
  geom_bar(aes(fill = Tenant), stat = "count", position = "dodge") +
  scale_fill_manual("Months", values=c("green","red","blue","black"),
                    labels=c("April","May","June","July")) +
  ylab("Count") +
  geom_text(aes(label = ..count..), stat = "count")

## Analysis 9.4 Relationship Between Area Type And Floor Number
ggplot(house_rent, aes(x = Area.Type, y = factor(FloorNum_Rev, levels = -2:76))) +
  geom_bar(aes(fill = Area.Type), stat = "identity", position = "dodge") +
  ylab("Floor Number")

# Question 10: Does Dates Have Anything To Do With Everything?
## Analysis 10.1 relationship between month and size
ggplot(house_rent, aes(x = Month, y = Size)) +
  geom_boxplot(aes(fill = Month))

## Analysis 10.2 relationship month with city
ggplot(house_rent, aes(x = City)) +
  geom_bar(aes(fill = Month)) +
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count")

## Analysis 10.3 relationship between month and rent
ggplot(house_rent, aes(x = Month, y = Rent)) +
  geom_point(aes(fill = Rent), position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_continuous(labels= comma)

ggplot(house_rent, aes(x = Month)) +
  geom_bar(aes(fill = Month), stat = "count", width = .4) +
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count")

## Analysis 10.4 Relationship Between Month And Tenant
ggplot(house_rent, aes(x = Month)) +
  geom_bar(aes(fill = Tenant), stat = "count") +
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count")
  
## Analysis 10.5 relationship month with floor
ggplot(house_rent, aes(x = Month)) +
  geom_bar(aes(fill = factor(FloorNum_Rev, levels = -2:76)), stat = "count") +
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count") +
  scale_fill_manual("Floor Numbers", values = factor(house_rent$FloorNum_Rev, levels = -2:76))

## Analysis 10.6 Relationship Between Month And Height Of Buildings
# house_rent <- house_rent[,!names(house_rent) %in% ("TotNumFloors")]

ggplot(house_rent, aes(x = factor(TotNumFloors, 0:89))) +
  geom_bar(aes(fill = Month), stat = "count") +
  geom_text(aes(label = ..count..), stat = "count") +
  ylab("Count") +
  xlab("Height Of Buildings") +
  scale_fill_manual("Months", values=c("green","red","blue","black"),
                    labels=c("April","May","June","July"))

