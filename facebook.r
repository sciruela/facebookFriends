require(RCurl)
require(rjson)

myaccess_token<-"YOUR_ACCESS_TOKEN"

facebook <-  function( path = "me", access_token = access_token, options){
    if( !missing(options) ){
        options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
    } else {
        options <- ""
    }
    data <- getURL( sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token ) )
    fromJSON( data )
}

dir.create( "/Users/sciruela/Documents/facebookFriends/photos" )
photos <- facebook( path="me/photos",access_token=myaccess_token )
sapply( photos$data, function(x){
    url <- x$source
    download.file( url, file.path( "/Users/sciruela/Documents/facebookFriends/photos", basename(url) ) )
})

# scrape the list of friends
friends <- facebook( path="me/friends" , access_token=myaccess_token)
# extract Facebook IDs
friends.id <- sapply(friends$data, function(x) x$id)
# extract names 
friends.name <- sapply(friends$data, function(x)  iconv(x$name,"UTF-8","ASCII//TRANSLIT"))
# short names to initials 
initials <- function(x) paste(substr(x,1,1), collapse="")
friends.initial <- sapply(strsplit(friends.name," "), initials) 

# short names to initials 
initials <- function(x) paste(substr(x,1,1), collapse="")
friends.initial <- sapply(strsplit(friends.name," "), initials) 

# friendship relation matrix
N <- length(friends.id)
friendship.matrix <- matrix(0,N,N)
for (i in 1:N) {
	tmp <- facebook( path=paste("me/mutualfriends", friends.id[i], sep="/") , access_token=myaccess_token)
	mutualfriends <- sapply(tmp$data, function(x) x$id)
	friendship.matrix[i,friends.id %in% mutualfriends] <- 1
}

require(igraph)

g<-graph.adjacency(friendship.matrix,mode="undirected")

V(g)$label<-friends.name

# Create fixed layout:
set.seed(1)
L <- layout.fruchterman.reingold(g)

# Rescale the layout to -1 to 1
L[,1]=(L[,1]-min(L[,1]))/(max(L[,1])-min(L[,1]))*2-1
L[,2]=(L[,2]-min(L[,2]))/(max(L[,2])-min(L[,2]))*2-1

V(g)$label<-friends.name




# ellipse graph with initials
pdf(file="/Users/sciruela/Documents/facebookFriends/facebook1.pdf", width=25, height=25)
plot(g,layout=L,vertex.size=0,vertex.frame.color="#00000000")
dev.off()


require(pixmap)

require(ReadImages)

# download small profile picture of each friend
dir.create("/Users/sciruela/Documents/facebookFriends/photos2")

for (i in 1:length(friends.id))
download.file(paste("http://graph.facebook.com", friends.id[i], "picture", sep="/"),destfile=paste("/Users/sciruela/Documents/facebookFriends/photos2/",friends.id[i],".jpg",sep=""))
 

system('for i in `ls /Users/sciruela/Documents/facebookFriends/photos2/*.jpg`; do j=${i%.*}; convert $j.jpg $j.jpg; done', wait=TRUE)


V(g)$label=""

l<-L
pdf(file="/Users/sciruela/Documents/facebookFriends/facebook2.pdf", width=25, height=25)
plot(g,layout=L,vertex.size=0,vertex.frame.color="#00000000")
for(i in length(friends.id):1){			  
 	myfriend.photo<-read.jpeg(paste("/Users/sciruela/Documents/facebookFriends/photos2/",friends.id[i],".jpg",sep=""))
	if(i!=1){
	apply(l,1,function(x)rasterImage(myfriend.photo,x[1]-0.05,x[2]-0.05,x[1]+0.05,x[2]+0.05))

	l<-l[-i,]
	}else{
		rasterImage(myfriend.photo,l[1]-0.05,l[2]-0.05,l[1]+0.05,l[2]+0.05)
	}
}
dev.off()

