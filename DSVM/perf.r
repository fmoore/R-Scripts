#library("ROCR", lib.loc="~/R/win-library/3.3")
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
roc_pred <- prediction(range01(scores$value), scores$conclusion)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
auc <- performance(roc_pred, measure = "auc")
print(paste("AUC: ", perf@y.values))

#library("pROC", lib.loc="~/R/win-library/3.3")
roc_smooth <- roc(scores$conclusion, range01(scores$value), direction="<")
plot(roc_smooth, col="red", lwd=3, main="ROC", legacy.axes = TRUE)
coord_smooth <- t(coords(roc_smooth, seq(0, 1, 0.005)))

#library('ggplot2')
# AUC ggplot function
fun.auc.ggplot <- function(pred_first, obs_first, title, pred_second = NULL, obs_second = NULL, addPoints = FALSE) {
  
  # pred_first = predicted values
  # obs_first = observed values (truth)
  # title = plot title
  # addPoints = points on ROC?
  
  # Run the AUC calculations
  ROC_perf_first <- performance(prediction(pred_first,obs_first),"tpr","fpr")
  ROC_auc_first <- performance(prediction(pred_first,obs_first),"auc")
  if (!is.null(pred_second) && !is.null(obs_second)) {
    ROC_perf_second <- performance(prediction(pred_second,obs_second),"tpr","fpr")
    ROC_auc_second <- performance(prediction(pred_second,obs_second),"auc")
  }
  # First ROC best
  roc_smooth_first <- roc(obs_first, pred_first, direction="<")
  # CI first
  rocobj_first <- roc(obs_first, pred_first, add=TRUE, percent=roc_smooth_first$percent)
  ci_first = capture.output(ci.auc(rocobj_first))
  coord_best_first <- t(coords(roc_smooth_first, "best"))
  print(coord_best_first)
  senspecbest_first <- c(paste("(", round(coord_best_first[1, 2],digits=2), ",", round(coord_best_first[1, 3],digits=2),")"))
  coord_smooth_first <- t(coords(roc_smooth_first, seq(0, 1, 0.001)))
  # Make first plot best
  plotbest_first <- data.frame(FP=ROC_perf_first@x.values[[1]],TP=ROC_perf_first@y.values[[1]],CUT=ROC_perf_first@alpha.values[[1]],POINT=NA)
  plotbest_first[unlist(lapply(c(coord_best_first[1,1]), function(x){which.min(abs(plotbest_first$CUT-x))})),"POINT"] <- c(coord_best_first[1,1])
  if (!is.null(pred_second) && !is.null(obs_second)) {
    # Second ROC best
    roc_smooth_second <- roc(obs_second, pred_second, direction="<")
    # CI second
    rocobj_second <- roc(obs_second, pred_second, add=TRUE, percent=roc_smooth_second$percent)
    ci_second = capture.output(ci.auc(rocobj_second))
    coord_best_second <- t(coords(roc_smooth_second, "best"))
    print(coord_best_second)
    senspecbest_second <- c(paste("(", round(coord_best_second[1, 2],digits=2), ",", round(coord_best_second[1, 3],digits=2),")"))
    coord_smooth_second <- t(coords(roc_smooth_second, seq(0, 1, 0.005)))
    # Make second plot best
    plotbest_second <- data.frame(FP=ROC_perf_second@x.values[[1]],TP=ROC_perf_second@y.values[[1]],CUT=ROC_perf_second@alpha.values[[1]],POINT=NA)
    plotbest_second[unlist(lapply(c(coord_best_second[1,1]), function(x){which.min(abs(plotbest_second$CUT-x))})),"POINT"] <- c(coord_best_second[1,1])
  }
  # Make first ROC plot data
  indices_first <- c(match(c(round(coord_best_first[1, 1],digits=2)), coord_smooth_first[,1]))
  cut_points_first <- c(round(coord_best_first[1, 1],digits=2))
  senspecbest_first <- c(paste("(", round(coord_smooth_first[indices_first[1], 2],digits=2), ",", round(coord_smooth_first[indices_first[1], 3],digits=2),")"))
  # First ROC
  plotdat_first <- data.frame(FP=ROC_perf_first@x.values[[1]],TP=ROC_perf_first@y.values[[1]],CUT=ROC_perf_first@alpha.values[[1]],POINT=NA)
  plotdat_first[unlist(lapply(cut_points_first, function(x){which.min(abs(plotdat_first$CUT-x))})),"POINT"] <- cut_points_first
  if (!is.null(pred_second) && !is.null(obs_second)) {
    # Make second ROC plot data
    indices_second <- c(match(c(round(coord_best_second[1, 1],digits=2)), coord_smooth_second[,1]))
    cut_points_second <- c(round(coord_best_second[1, 1],digits=2))
    senspecbest_second <- c(paste("(", round(coord_smooth_second[indices_second[1], 2],digits=2), ",", round(coord_smooth_second[indices_second[1], 3],digits=2),")"))
    # Second ROC
    plotdat_second <- data.frame(FP=ROC_perf_second@x.values[[1]],TP=ROC_perf_second@y.values[[1]],CUT=ROC_perf_second@alpha.values[[1]],POINT=NA)
    plotdat_second[unlist(lapply(cut_points_second, function(x){which.min(abs(plotdat_second$CUT-x))})),"POINT"] <- cut_points_second
  }
  # Plot the curve
  if (addPoints) {
    if (!is.null(pred_second) && !is.null(obs_second)) {
      ggplot() +
        geom_point(data=plotdat_first, aes(x=FP,y=TP,col=TP), size=0.01, , colour = "lightblue") +
        geom_point(data=plotdat_second, aes(x=FP,y=TP,col=TP), size=0.5) +
        scale_color_gradientn(colours = rainbow(10)) +
        #geom_abline(intercept=0,slope=1) +
        #geom_line(lwd=0.5) + 
        geom_point(data=plotdat_first[!is.na(plotbest_first$POINT),], aes(x=FP,y=TP,fill=POINT), pch=21, size=1, col="black") +
        geom_text(data=plotbest_first[!is.na(plotbest_first$POINT),], aes(x=FP,y=TP,fill=POINT), label=senspecbest_first, size=3, hjust=0.5, vjust=1, col="black") +
        geom_point(data=plotdat_second[!is.na(plotbest_second$POINT),], aes(x=FP,y=TP,fill=POINT), pch=21, size=1, col="black") +
        geom_text(data=plotbest_second[!is.na(plotbest_second$POINT),], aes(x=FP,y=TP,fill=POINT), label=senspecbest_second, size=3, hjust=0.5, vjust=1, col="black") +
        scale_fill_gradientn("Threshold Cutoff",colours=rainbow(14)[1:11]) +
        scale_x_continuous("False Positive Rate", limits=c(0,1)) +
        scale_y_continuous("True Positive Rate", limits=c(0,1)) +
        annotate("text",x=0.86,y=0.36,label=(paste("Accuracy = ",round((coord_best_second[1,2] + coord_best_second[1,3])/2.0,digits=3),sep=" ")),hjust=1, size=4, fontface=2,colour="blue") +
        annotate("text",x=0.97,y=0.32,label=paste("AUC Grader = ",round(ROC_auc_second@y.values[[1]],digits=2),"with",sub("\\(DeLong\\)", "", ci_second),sep=" "),hjust=1, size=4) +
        annotate("text",x=0.97,y=0.28,label=paste("AUC All Graders = ",round(ROC_auc_first@y.values[[1]],digits=2),"with",sub("\\(DeLong\\)", "", ci_first),sep=" "),hjust=1, size=4) +
        theme(legend.position="none", plot.title=element_text(vjust=2)) +
        ggtitle(title)
    } else {
      ggplot() +
        geom_point(data=plotdat_first, aes(x=FP,y=TP,col=TP), size=0.5) +
        scale_colour_gradientn("",colours=rainbow(14)[1:11]) +
        geom_abline(intercept=0,slope=1) +
        geom_line(lwd=1) +
        geom_point(data=plotbest_first[!is.na(plotdat_first$POINT),], aes(x=FP,y=TP,fill=POINT), pch=21, size=3, col="black") +
        geom_text(data=plotbest_first[!is.na(plotdat_first$POINT),], aes(x=FP,y=TP,fill=POINT), label=senspecbest_first, size=3, hjust=0.5, vjust=1, col="red") +
        scale_fill_gradientn("Threshold Cutoff",colours=rainbow(14)[1:11]) +
        scale_x_continuous("False Positive Rate", limits=c(0,1)) +
        scale_y_continuous("True Positive Rate", limits=c(0,1)) +
        annotate("text",x=0.97,y=0.11,label=paste("AUC = ",round(ROC_auc_first@y.values[[1]],digits=2),"with",sub("\\(DeLong\\)", "", ci_first),sep=" "),hjust=1, size=3.4) +
        theme(legend.position="none", plot.title=element_text(vjust=2)) +
        ggtitle(title)
    }
  } else {
    if (!is.null(pred_second) && !is.null(obs_second)) {
      ggplot() +
        geom_point(data=plotdat_first, aes(x=FP,y=TP,col=TP), size=0.5) +
        geom_point(data=plotdat_second, aes(x=FP,y=TP,col=TP), size=0.5) +
        scale_colour_gradientn("",colours=rainbow(14)[1:11]) +
        geom_abline(intercept=0,slope=1) +
        geom_line(lwd=1) + 
        scale_fill_gradientn("Threhsold Cutoff",colours=rainbow(14)[1:11]) +
        scale_x_continuous("False Positive Rate", limits=c(0,1)) +
        scale_y_continuous("True Positive Rate", limits=c(0,1)) +
        annotate("text",x=0.97,y=0.15,label=paste("AUC1 = ",round(ROC_auc_second@y.values[[1]],digits=2),"with",sub("\\(DeLong\\)", "", ci_first),sep=" "),hjust=1, size=3.4) +
        annotate("text",x=0.97,y=0.11,label=paste("AUC2 = ",round(ROC_auc_first@y.values[[1]],digits=2),"with",sub("\\(DeLong\\)", "", ci_first),sep=" "),hjust=1, size=3.4) +
        theme(legend.position="none", plot.title=element_text(vjust=2)) +
        ggtitle(title)
    } else {
      ggplot() +
        geom_point(data=plotdat_first, aes(x=FP,y=TP,col=TP), size=0.5) +
        scale_colour_gradientn("",colours=rainbow(14)[1:11]) +
        geom_abline(intercept=0,slope=1) +
        geom_line(lwd=1) + 
        scale_fill_gradientn("Threhsold Cutoff",colours=rainbow(14)[1:11]) +
        scale_x_continuous("False Positive Rate", limits=c(0,1)) +
        scale_y_continuous("True Positive Rate", limits=c(0,1)) +
        annotate("text",x=0.97,y=0.11,label=paste("AUC = ",round(ROC_auc_first@y.values[[1]],digits=2),"with",sub("\\(DeLong\\)", "", ci_first),sep=" "),hjust=1, size=3.4) +
        theme(legend.position="none", plot.title=element_text(vjust=2)) +
        ggtitle(title)
    }
  }
}

nomod = subset(classificationResults, classificationResults[,7] != "DR Moderate")

fun.auc.ggplot(range01(scores$value), scores$conclusion, "AUC Plot")
