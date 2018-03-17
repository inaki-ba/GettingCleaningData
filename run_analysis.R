run_analysis <- function() {
	
	## Sets up vectors, including activity lables, and subsets of variables: means and stds
	l_activities <-read.table("activity_labels.txt")
	t_activities<-read.table("features.txt")
	i_activities<-grep("mean|std", t_activities$V2)
	s_activities<-grep("mean|std", t_activities$V2, value = TRUE)
	
	## Shapes train sets, including column nanes, activity names for the activiy variable
	t_train<-read.table("train/X_train.txt")
	t_train_lab<-read.table("train/y_train.txt")
	t_train_sub<-read.table("train/subject_train.txt")
	d_train<-data.frame(t_train_sub, t_train_lab, t_train[,i_activities])
	colnames(d_train) <- c("Subject", "Activity", c(s_activities))
	d_train$Activity<-l_activities$V2[d_train$Activity]
	
	## Shapes test sets, including column nanes, activity names for the activiy variable
	t_test<-read.table("test/X_test.txt")
	t_test_lab<-read.table("test/y_test.txt")
	t_test_sub<-read.table("test/subject_test.txt")
	d_test<-data.frame(t_test_sub, t_test_lab, t_test[,i_activities])
	colnames(d_test) <- c("Subject", "Activity", c(s_activities))
	d_test$Activity<-l_activities$V2[d_test$Activity]
	
	##	Merges the two sets, and calculates the summary set with averages per activity/subject
	final_frame<-rbind("Train"=d_train, "Test"=d_test)
	k<-1
	final_frame2<-final_frame[1,]
	for( i in 1:nrow(l_activities)) {
		for(j in 1:30) {
			subs<-final_frame$Subject==j & final_frame$Activity==l_activities[i,2]
			sub_frame <- sapply( final_frame[subs,3:ncol(final_frame)], function(x) mean(x))
			final_frame2[k,] = c(j, as.character(l_activities[i,2]), sub_frame)
			k<-k+1
		}
	}

	##	Returns the summary set: 180 rows, 81 columns
	final_frame2
}