Codebook 
==================================

## Variables
````
Activity                    
    Acitivity label
                           "WALKING"
                           "WALKING_UPSTAIRS"
                           "WALKING_DOWNSTAIRS"
                           "SITTING"
                           "STANDING"
                           "LAYING"
                           
subject                    
    Subject number
                           1..30 .Unique identifier assigned to each subject

"TimeDomain_BodyAccelerometer_Mean_X"                         
"TimeDomain_BodyAccelerometer_Mean_Y"                        
"TimeDomain_BodyAccelerometer_Mean_Z"                         
"TimeDomain_GravityAccelerometer_Mean_X"                     
""TimeDomain_GravityAccelerometer_Mean_Y"                      
"TimeDomain_GravityAccelerometer_Mean_Z"                     
"TimeDomain_BodyAccelerometerJerk_Mean_X"                     
"TimeDomain_BodyAccelerometerJerk_Mean_Y"                    
"TimeDomain_BodyAccelerometerJerk_Mean_Z"                     
"TimeDomain_BodyGyroscope_Mean_X"                            
"TimeDomain_BodyGyroscope_Mean_Y"                             
"TimeDomain_BodyGyroscope_Mean_Z"                            
"TimeDomain_BodyGyroscopeJerk_Mean_X"                         
"TimeDomain_BodyGyroscopeJerk_Mean_Y"                        
"TimeDomain_BodyGyroscopeJerk_Mean_Z"                         
"TimeDomain_BodyAccelerometerMagnitude_mean"                 
"TimeDomain_GravityAccelerometerMagnitude_mean"               
"TimeDomain_BodyAccelerometerJerkMagnitude_mean"             
"TimeDomain_BodyGyroscopeMagnitude_mean"                      
"TimeDomain_BodyGyroscopeJerkMagnitude_mean"                 
"FrequencyDomain_BodyAccelerometer_Mean_X"                    
"FrequencyDomain_BodyAccelerometer_Mean_Y"                   
"FrequencyDomain_BodyAccelerometer_Mean_Z"                    
"FrequencyDomain_BodyAccelerometer_meanFreq_X"               
"FrequencyDomain_BodyAccelerometer_meanFreq_Y"                
"FrequencyDomain_BodyAccelerometer_meanFreq_Z"               
"FrequencyDomain_BodyAccelerometerJerk_Mean_X"
"FrequencyDomain_BodyAccelerometerJerk_Mean_Y"               
"FrequencyDomain_BodyAccelerometerJerk_Mean_Z"               
"FrequencyDomain_BodyAccelerometerJerk_meanFreq_X"           
"FrequencyDomain_BodyAccelerometerJerk_meanFreq_Y"            
"FrequencyDomain_BodyAccelerometerJerk_meanFreq_Z"           
"FrequencyDomain_BodyGyroscope_Mean_X"                        
"FrequencyDomain_BodyGyroscope_Mean_Y"                       
"FrequencyDomain_BodyGyroscope_Mean_Z"                        
"FrequencyDomain_BodyGyroscope_meanFreq_X"                   
"FrequencyDomain_BodyGyroscope_meanFreq_Y"                    
"FrequencyDomain_BodyGyroscope_meanFreq_Z"                   
"FrequencyDomain_BodyAccelerometerMagnitude_mean"             
"FrequencyDomain_BodyAccelerometerMagnitude_meanFreq"        
"FrequencyDomain_BodyBodyAccelerometerJerkMagnitude_mean"     
"FrequencyDomain_BodyBodyAccelerometerJerkMagnitude_meanFreq"
"FrequencyDomain_BodyBodyGyroscopeMagnitude_mean"             
"FrequencyDomain_BodyBodyGyroscopeMagnitude_meanFreq"        
"FrequencyDomain_BodyBodyGyroscopeJerkMagnitude_mean"         
"FrequencyDomain_BodyBodyGyroscopeJerkMagnitude_meanFreq"
````
'
## Aggregation and Transformation

The set of variables that were estimated from mean and Freq:

All the values are means, aggregated over 30 subjects and 6 activities, hence the resulting dataset is 180 rows.