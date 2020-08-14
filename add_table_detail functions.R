

library(xml2)
library(XML)
library(dplyr)
library(knitr)
library(kableExtra)
library(roxygen2)


# Would like to get to the stage where i can change the information inside xml.

add_table_detail <- function(kable_table, row, column, comments){


  if(length(row) != length(comments) | length(column) != length(comments)){
    
    stop("Remember you need to specify a comment per cel. 
         The length for the row the columns and the comments should all be the same")
  }
  
  xml_table <- kable_as_xml(kable_table)

  for(i in 1:length(row)){
    
  letter <- as.character(letters[i])

  xml_table %>%  
    xml_child(2) %>% 
    xml_child(row[i]) %>%
    xml_child(column[i])%>% 
    xml_add_child("sup", letter) 

  }

  return(xml_as_kable(xml_table) %>% 
           footnote(alphabet = comments))

}


add_table_detail_red <- function(kable_table, row, column, comments){
  
  
  if(length(row) != length(comments) | length(column) != length(comments)){
    
    stop("Remember you need to specify a comment per cel. 
         The length for the row the columns and the comments should all be the same")
  }
  
  xml_table <- kable_as_xml(kable_table)
  
  for(i in 1:length(row)){
    
    letter <- as.character(letters[i])
    
    xml_table %>%  
      xml_child(2) %>% 
      xml_child(row[i]) %>%
      xml_child(column[i])%>%
      xml_set_attr("style", "color: red;")

    xml_table %>%  
      xml_child(2) %>% 
      xml_child(row[i]) %>%
      xml_child(column[i])%>% 
      xml_add_child("sup", letter) 
    
  }
  
  return(xml_as_kable(xml_table) %>% 
           footnote(alphabet = comments))
  
}

# Example below


mtcars[1:4, 1:4] %>% 
  kable(escape = F) %>% 
  kable_styling() %>% 
  add_table_detail_red(c(1,3), c(1,1), c("Poor car", "Better Car"))


# Improvements: Seems to be a space between the superscript and the text in the table
# May be worth trimming the text either side of the text? 
# Other wise this does seem to work okay in fairness at the moment .













