run_analysis <- function() {
	
	##
	l_activities <-read.table("wear/activity_labels.txt")
	t_activities<-read.table("wear/features.txt")
	i_activities<-grep("mean|std", t_activities$V2)
	s_activities<-grep("mean|std", t_activities$V2, value = TRUE)
	
	##
	t_train<-read.table("wear/train/X_train.txt")
	t_train_lab<-read.table("wear/train/y_train.txt")
	t_train_sub<-read.table("wear/train/subject_train.txt")
	d_train<-data.frame(t_train_sub, t_train_lab, t_train[,i_activities])
	colnames(d_train) <- c("Subject", "Activity", c(s_activities))
	d_train$Activity<-l_activities$V2[d_train$Activity]
	
	##
	t_test<-read.table("wear/test/X_test.txt")
	t_test_lab<-read.table("wear/test/y_test.txt")
	t_test_sub<-read.table("wear/test/subject_test.txt")
	d_test<-data.frame(t_test_sub, t_test_lab, t_test[,i_activities])
	colnames(d_test) <- c("Subject", "Activity", c(s_activities))
	d_test$Activity<-l_activities$V2[d_test$Activity]
	
	##	
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
	final_frame2
}