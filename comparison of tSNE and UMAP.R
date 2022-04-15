data("iris")


iris.data<-iris[,1:4]


iris.pca=prcomp(iris.data, scale= T)

dsq<-iris.pca$sdev^2
cumsum(dsq)/sum(dsq)

iris.pca.df<-data.frame(iris.pca$x,Species=iris$Species)

library(ggplot2)
library(patchwork)
p.iris.pca<-ggplot(iris.pca.df,
                   aes(x=PC1,y=PC2,color=Species))+geom_point()+
  stat_ellipse(level = 0.95,show.legend = T)+
  annotate('text', label = 'setosa', x = -2, y = -1.25, size = 3, colour = 'black') +
  annotate('text', label = 'versicolor', x = -0.2, y = - 0.7, size = 3, colour = 'black') +
  annotate('text', label = 'virginica', x = 2, y = 0.5, size = 3, colour = 'black')+
  labs(title = "iris PCA plot")+
  theme(plot.title = element_text(hjust = 0.5))


library(Rtsne)
set.seed(123)
iris.tsne<-Rtsne(iris.data,pca=FALSE,pca_scale=TRUE, 
                 dims=2,check_duplicates=FALSE)



iris.tsne.df<-data.frame(iris.tsne$Y,Species=iris$Species)



p.iris.tsne<-ggplot(iris.tsne.df,
                    aes(x=X1,y=X2,color=Species))+geom_point()+
  stat_ellipse(level = 0.95,show.legend = T)+
  annotate('text', label = 'setosa', x = 15, y = -1, size = 3, colour = 'black') +
  annotate('text', label = 'versicolor', x = 1, y = 2, size = 3, colour = 'black') +
  annotate('text', label = 'virginica', x = -15, y = -1, size = 3, colour = 'black')+
  labs(title = "iris t-SNE plot",x="Y1",y="Y2")+
  theme(plot.title = element_text(hjust = 0.5))



library(umap)
set.seed(123)
iris.umap<-umap(iris.data)

iris.umap.df<-data.frame(iris.umap$layout,Species=iris$Species)



p.iris.umap<-ggplot(iris.umap.df,
                    aes(x=X1,y=X2,color=Species))+geom_point()+
  stat_ellipse(level = 0.95,show.legend = T)+
  annotate('text', label = 'setosa', x = 12.5, y = 0, size = 3, colour = 'black') +
  annotate('text', label = 'versicolor', x = -4, y = -5, size = 3, colour = 'black') +
  annotate('text', label = 'virginica', x = -7, y = 5, size = 3, colour = 'black')+
  labs(title = "iris UMAP plot")+theme(plot.title = element_text(hjust = 0.5))


#########################

texture=read.table("C:/Users/cuisa/Desktop/Texture.csv", sep=",", header=T)

texture.pca=prcomp(texture[,-41], scale= T)

dsq<-texture.pca$sdev^2
cumsum(dsq)/sum(dsq)


texture.pca.df<-data.frame(texture.pca$x, 
                           Class=factor(texture$A41))


p.texture.pca<-ggplot(texture.pca.df,
                      aes(x=PC1,y=PC2,color=Class))+geom_point()+
  labs(title = "image texture PCA plot")+
  theme(plot.title = element_text(hjust = 0.5))


set.seed(123)
texture.tsne<-Rtsne(texture[,-41],pca=FALSE,pca_scale=TRUE, 
                    dims=2,check_duplicates=FALSE)


texture.tsne.df<-
  data.frame(texture.tsne$Y,Class=factor(texture$A41))


p.texture.tsne<-ggplot(texture.tsne.df,
                       aes(x=X1,y=X2,color=Class))+geom_point()+
  annotate('text', label = '2', x = 21, y = -16, size = 3, colour = 'black') +
  annotate('text', label = '3', x = -12.5, y = -20, size = 3, colour = 'black')+ 
  annotate('text', label = '4', x = 12.5, y = -40, size = 3, colour = 'black')+
  annotate('text', label = '6', x = 5, y = 20, size = 3, colour = 'black')+
  annotate('text', label = '7', x = -12.5, y = 37.5, size = 3, colour = 'black')+
  annotate('text', label = '8', x = -15, y = 0, size = 3, colour = 'black')+
  annotate('text', label = '9', x = 6, y = -10, size = 3, colour = 'black')+
  annotate('text', label = '10', x = -12.5, y = 10.5, size = 3, colour = 'black')+
  annotate('text', label = '12', x = 37.5, y = 0, size = 3, colour = 'black')+
  annotate('text', label = '13', x = 16.8, y = 33, size = 3, colour = 'black')+
  annotate('text', label = '14', x = -35, y = -12.5, size = 3, colour = 'black')+
  labs(title = "image texture t-SNE plot")+theme(plot.title = element_text(hjust = 0.5))




set.seed(123)
texture.umap<-umap(texture[,-41])

texture.umap.df<-
  data.frame(texture.umap$layout,Class=factor(texture$A41))


p.texture.umap<-ggplot(texture.umap.df,
                       aes(x=X1,y=X2,color=Class))+geom_point()+
  annotate('text', label = '2', x = -7, y = 1.6, size = 3, colour = 'black') +
  annotate('text', label = '3', x = -5.8, y = -2.9, size = 3, colour = 'black')+ 
  annotate('text', label = '4', x = -13.7, y = 0.4, size = 3, colour = 'black')+
  annotate('text', label = '6', x = -2.3, y = 9, size = 3, colour = 'black')+
  annotate('text', label = '7', x = 19.7, y = 1.2, size = 3, colour = 'black')+
  annotate('text', label = '8', x = 0, y = 2.5, size = 3, colour = 'black')+
  annotate('text', label = '9', x = -4.2, y = 0.4, size = 3, colour = 'black')+
  annotate('text', label = '10', x = 1.4, y = 4.4, size = 3, colour = 'black')+
  annotate('text', label = '12', x = -1, y = -11.6, size = 3, colour = 'black')+
  annotate('text', label = '13', x = 10, y = 1.2, size = 3, colour = 'black')+
  annotate('text', label = '14', x = 0.6, y = -4.1, size = 3, colour = 'black')+
  labs(title = "image texture UMAP plot")+theme(plot.title = element_text(hjust = 0.5))





# knn #
library(class)
set.seed(1234)
index<-sample(1:2,replace = T, size= 5500, prob = c(0.75,0.25))


test.error.raw<-NULL
for (i in 1:100) {
  knn.raw <- knn(texture[index==1,-41],
                 texture[index==2,-41],
                 texture[index==1,41], k=i, 
                 prob=TRUE)
  
  test.error.raw[i]<-1-mean(knn.raw==texture[index==2,41])
}

which.min(test.error.raw)
test.error.raw[which.min(test.error.raw)]



test.error.pca.2pc<-NULL
for (i in 1:100) {
  knn.pca <- knn(texture.pca.df[index==1,c(1:2)],
                 texture.pca.df[index==2,c(1:2)],
                 texture.pca.df[index==1,41], k=i, 
                 prob=TRUE)
  
  test.error.pca.2pc[i]<-
    1-mean(knn.pca==texture.pca.df[index==2,41])
}

which.min(test.error.pca.2pc)
test.error.pca.2pc[which.min(test.error.pca.2pc)]



test.error.pca.4pc<-NULL
for (i in 1:100) {
  knn.pca <- knn(texture.pca.df[index==1,c(1:4)],
                 texture.pca.df[index==2,c(1:4)],
                 texture.pca.df[index==1,41], k=i, 
                 prob=TRUE)
  
  test.error.pca.4pc[i]<-
    1-mean(knn.pca==texture.pca.df[index==2,41])
}

which.min(test.error.pca.4pc)
test.error.pca.4pc[which.min(test.error.pca.4pc)]


test.error.pca.6pc<-NULL
for (i in 1:100) {
  knn.pca <- knn(texture.pca.df[index==1,c(1:6)],
                 texture.pca.df[index==2,c(1:6)],
                 texture.pca.df[index==1,41], k=i, 
                 prob=TRUE)
  
  test.error.pca.6pc[i]<-
    1-mean(knn.pca==texture.pca.df[index==2,41])
}

which.min(test.error.pca.6pc)
test.error.pca.6pc[which.min(test.error.pca.6pc)]




test.error.tsne<-NULL
for (i in 1:100) {
  knn.tsne <- knn(texture.tsne.df[index==1,c(1:2)],
                  texture.tsne.df[index==2,c(1:2)],
                  texture.tsne.df[index==1,3], k=i, 
                  prob=TRUE)
  
  test.error.tsne[i]<-
    1-mean(knn.tsne==texture.tsne.df[index==2,3])
}

which.min(test.error.tsne)
test.error.tsne[which.min(test.error.tsne)]



test.error.umap<-NULL
for (i in 1:100) {
  knn.umap <- knn(texture.umap.df[index==1,c(1:2)],
                  texture.umap.df[index==2,c(1:2)],
                  texture.umap.df[index==1,3], k=i, 
                  prob=TRUE)
  
  test.error.umap[i]<-
    1-mean(knn.umap==texture.umap.df[index==2,3])
}

which.min(test.error.umap)
test.error.umap[which.min(test.error.umap)]

######### scRNA data

library(splatter)

params<- newSplatParams()
params<- setParams(params, batchCells=5000, de.prob = 0.06,  de.facScale = 0.5, seed=1)
sim<- splatSimulate(params, nGenes = 15000, group.prob = c(0.25, 0.2,0.15,0.1,0.08,0.07,0.05,0.04,0.03,0.03), 
                    method = "groups")

simdata<-as.data.frame(counts(sim))
cell.info<-as.data.frame(colData(sim))


sim.work<-t(as.matrix(log(simdata+1)))
zero.counts<-colSums(sim.work)
sim.work<-sim.work[,-which(zero.counts==0)]


sim.pca=prcomp(sim.work,scale= T)



dsq<-sim.pca$sdev^2
cumsum(dsq)/sum(dsq)


sim.pca.df<-data.frame(sim.pca$x, 
                           Class=factor(cell.info$Group))


p.sim.pca<-ggplot(sim.pca.df,
                      aes(x=PC1,y=PC2,color=Class))+geom_point()+
  labs(title = "Simulated scRNAseq PCA plot")+
  theme(plot.title = element_text(hjust = 0.5))





set.seed(123)
sim.tsne<-Rtsne(sim.work, pca=T ,pca_scale=TRUE, initial_dims=50,
                    dims=2, check_duplicates=FALSE)


sim.tsne.df<-
  data.frame(sim.tsne$Y,Class=factor(cell.info$Group))


p.sim.tsne<-ggplot(sim.tsne.df,
                       aes(x=X1,y=X2,color=Class))+geom_point()+
  annotate('text', label = 'Group 1', x = -9, y = 20, size = 4, colour = 'black') +
  annotate('text', label = 'Group 2', x = -8, y = -19, size = 4, colour = 'black')+ 
  annotate('text', label = 'Group 3', x = 25, y = -9, size = 4, colour = 'black')+
  annotate('text', label = 'Group 4', x = -34, y = 6, size = 4, colour = 'black')+
  annotate('text', label = 'Group 5', x = 23, y = 25, size = 4, colour = 'black')+
  annotate('text', label = 'Group 6', x = 13, y = 5, size = 4, colour = 'black')+
  annotate('text', label = 'Group 7', x = 11, y = -24, size = 4, colour = 'black')+
  annotate('text', label = 'Group 8', x = 1, y = -40, size = 4, colour = 'black')+
  annotate('text', label = 'Group 9', x = -37, y = -12, size = 4, colour = 'black')+
  annotate('text', label = 'Group 10', x = -1, y = 4, size = 4, colour = 'black')+
  labs(title = "Simulated scRNAseq t-SNE plot")+theme(plot.title = element_text(hjust = 0.5))






set.seed(123)
sim.umap<-umap(sim.work)

sim.umap.df<-
  data.frame(sim.umap$layout, Class=factor(cell.info$Group))


p.sim.umap<-ggplot(sim.umap.df,
                       aes(x=X1,y=X2,color=Class))+geom_point()+
  annotate('text', label = 'Group 1', x = -2.5, y = 1, size = 4, colour = 'black') +
  annotate('text', label = 'Group 2', x = 1.6, y = -1.5, size = 4, colour = 'black')+ 
  annotate('text', label = 'Group 3', x = -1.35, y = -2.8, size = 4, colour = 'black')+
  annotate('text', label = 'Group 4', x = 3, y = 0.1, size = 4, colour = 'black')+
  annotate('text', label = 'Group 5', x = 2.5, y = 2.5, size = 4, colour = 'black')+
  annotate('text', label = 'Group 6', x = -0.6, y = 2.5, size = 4, colour = 'black')+
  annotate('text', label = 'Group 7', x = 0.65, y = 1.3, size = 4, colour = 'black')+
  annotate('text', label = 'Group 8', x = 1.25, y = 0.5, size = 4, colour = 'black')+
  annotate('text', label = 'Group 9', x = -0.1, y = 1.15, size = 4, colour = 'black')+
  annotate('text', label = 'Group 10', x = -1.25, y = 1.25, size = 4, colour = 'black')+
  labs(title = "Simulated scRNAseq UMAP plot")+theme(plot.title = element_text(hjust = 0.5))









# knn simulation #
library(class)
set.seed(1234)
index<-sample(1:2,replace = T, size= 5000, prob = c(0.75,0.25))


ind2<-sample(1:14943, size = 5000, replace = F)

test.error.raw<-NULL
for (i in 1:100) {
  knn.raw <- knn(sim.work[index==1,ind2],
                 sim.work[index==2,ind2],
                 cell.info$Group[index==1] , k=i, 
                 prob=TRUE)
  
  test.error.raw[i]<-1-mean(knn.raw==cell.info$Group[index==2])
}

which.min(test.error.raw)
test.error.raw[which.min(test.error.raw)]



set.seed(1234)
test.error.pca.2pc<-NULL
for (i in 1:100) {
  knn.pca <- knn(sim.pca.df[index==1,c(1:2)],
                 sim.pca.df[index==2,c(1:2)],
                 cell.info$Group[index==1], k=i, 
                 prob=TRUE)
  
  test.error.pca.2pc[i]<-
    1-mean(knn.pca==cell.info$Group[index==2])
}


which.min(test.error.pca.2pc)
test.error.pca.2pc[which.min(test.error.pca.2pc)]



set.seed(1234)
test.error.pca.4pc<-NULL
for (i in 1:100) {
  knn.pca <- knn(sim.pca.df[index==1,c(1:4)],
                 sim.pca.df[index==2,c(1:4)],
                 cell.info$Group[index==1], k=i, 
                 prob=TRUE)
  
  test.error.pca.4pc[i]<-
    1-mean(knn.pca==cell.info$Group[index==2])
}


which.min(test.error.pca.4pc)
test.error.pca.4pc[which.min(test.error.pca.4pc)]


set.seed(1234)
test.error.pca.6pc<-NULL
for (i in 1:100) {
  knn.pca <- knn(sim.pca.df[index==1,c(1:6)],
                 sim.pca.df[index==2,c(1:6)],
                 cell.info$Group[index==1], k=i, 
                 prob=TRUE)
  
  test.error.pca.6pc[i]<-
    1-mean(knn.pca==cell.info$Group[index==2])
}


which.min(test.error.pca.6pc)
test.error.pca.6pc[which.min(test.error.pca.6pc)]






set.seed(1234)
test.error.tsne<-NULL
for (i in 1:100) {
  knn.tsne <- knn(sim.tsne.df[index==1,c(1:2)],
                  sim.tsne.df[index==2,c(1:2)],
                  sim.tsne.df[index==1,3], k=i, 
                  prob=TRUE)
  
  test.error.tsne[i]<-
    1-mean(knn.tsne==sim.tsne.df[index==2,3])
}

which.min(test.error.tsne)
test.error.tsne[which.min(test.error.tsne)]



set.seed(1234)
test.error.umap<-NULL
for (i in 1:100) {
  knn.umap <- knn(sim.umap.df[index==1,c(1:2)],
                  sim.umap.df[index==2,c(1:2)],
                  sim.umap.df[index==1,3], k=i, 
                  prob=TRUE)
  
  test.error.umap[i]<-
    1-mean(knn.umap==sim.umap.df[index==2,3])
}

which.min(test.error.umap)
test.error.umap[which.min(test.error.umap)]











