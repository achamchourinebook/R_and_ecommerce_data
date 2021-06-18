image.multiply <- function(n_images, source_image, target_dir)
{
  # generating new images based on source_image
  # n_images:     number of new images to be generated
  # source_image: source image
  # target_dir:   target directory
  
  file <- basename(source_image)
  
  # we will consider the file name without the extension part as file id
  file_id <- substr(file,1,nchar(file)-4)
  
  image <- magick::image_read(source_image)
  info.orig <- magick::image_info(image)
  
  # trim the image 
  trim_image <- magick::image_trim(image)
  info.trim <- magick::image_info(trim_image)
  
  delta.w  <- (info.orig$width -info.trim$width)
  delta.h <- (info.orig$height -info.trim$height)
  
  # calculate what image we should save (based on n_images)
  n <- as.integer(delta.w*delta.h/n_images)
  
  # create blank image we will use for image generating
  blank <- magick::image_blank(info.orig$width,info.orig$height)
  
  j <- 1
  k <- 1
  for(w in 1:delta.w)
  {
    for(h in 1:delta.h)
    {
      if(k%%n==0)
      {
        offset <- paste0("+",w,"+",h)
        
        # note that composing with blank will result in png format
        new_image <- magick::image_composite(blank, trim_image, operator = "over", offset = offset)
        
        new_file_id <- paste0(file_id,"_",j)
        magick::image_write(new_image, path=paste0(target_dir,"/",new_file_id,".png"))
        
        print(new_file_id)
        
        j <- j +1
      }
      
      k <- k +1
    }
  }
}

# select every 100th image for the test set
n <- 100

testset.source <- "Lores/Orig/Train"
testset.target <- "Lores/Orig/Test"

files <- list.files(testset.source)
l <- length(files)

if(!dir.exists(testset.target))
{
  dir.create(testset.target, recursive = TRUE)
}

j <- 1
for(i in 1:l)
{
  if(i%%n!=0)
  {
    next  
  }
  
  oldName <- paste0(testset.source,"/",files[i])
  newName <- paste0(testset.target,"/",files[i])
  
  print(paste0("Processing ",files[i],"... Processed ",j-1))
  
  tryCatch(
    {
      file.copy(oldName,newName)
      file.remove(oldName)
    }  
  )
  
  j <- j+1
}

# image pre-processing 
source("Modules/dbData.R")

catimages <- db.data(db="AdventureWear", vw="vwStyles")
str(catimages)

# convert to data.table, these two steps can be skipped
# install.packages("data.table")
catimages <- data.table::setDT(catimages)
# build an index
data.table::setkey(catimages, id)

str(catimages)

isize <- c(45,60)

trainset.source <- "Lores/Orig/Train"
trainset.target <- "Lores/Train"

files <- list.files(trainset.source)
l <- length(files)

for(i in 1:l)
{
  print(paste0("Processing ",files[i],"... Processed ",i-1))
  
  image <- magick::image_read(paste0(trainset.source,"/",files[i]))
  
  # scale to size
  image <- magick::image_scale(image, isize[1])
  
  # find category
  fileid <- substr(files[i],1,nchar(files[i])-4)
  
  category <- catimages[id==fileid,category]
  
  fdir <- paste0(trainset.target, "/", category)
  
  if(!dir.exists(fdir))
  {
    dir.create(fdir, recursive = TRUE)
  }
  
  # save
  magick::image_write(image, path=paste0(fdir,"/",files[i]))
}

# prepare test images
testset.source <- "Lores/Orig/Test"
testset.target <- "Lores/Test"

# test category is fixed (a dummy one, necessary to match training set structure)
category <- "Dummy"

fdir <- paste0(testset.target, "/", category)

if(!dir.exists(fdir))
{
  dir.create(fdir, recursive = TRUE)
}

files <- list.files(testset.source)
l <- length(files)

for(i in 1:l)
{
  print(paste0("Processing ",files[i],"... Processed ",i-1))
  
  image <- magick::image_read(paste0(testset.source,"/",files[i]))
  
  # scale to size
  image <- magick::image_scale(image, isize[1])
  
  # save
  magick::image_write(image, path=paste0(fdir,"/",files[i]))
}

# model training
library(dplyr)
library(keras)

size <- c(45,60)
path.train <- "Lores/Train"
batch_size <- 32

# note the use of sparse instead of categorical
generator.train <- keras::flow_images_from_directory(
  directory = path.train, 
  target_size = size,
  #  class_mode = "categorical",
  class_mode = "sparse",
  batch_size = batch_size,
  shuffle = TRUE)

generator.train$class_indices

nsamples.train <- generator.train$n
output_size <- length(generator.train$class_indices)

model <- keras::keras_model_sequential()

model %>%
  keras::layer_conv_2d(filters = 10,
                       kernel_size = c(5,5),
                       input_shape = c(45,60,3),
                       activation = "relu") %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  keras::layer_conv_2d(filter = 20,
                       kernel_size = c(5,5),
                       activation = "relu") %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  keras::layer_flatten() %>%
  keras::layer_dense(units=100, activation = "relu") %>%
  keras::layer_dense(units=output_size, activation = "softmax")

model %>% keras::compile(loss="sparse_categorical_crossentropy",
                         optimizer="adam",
                         metrics="accuracy")

model

epochs <- 10

Sys.time()
history <- model %>% 
  keras::fit_generator(generator = generator.train,
                       steps_per_epoch = as.integer(nsamples.train/batch_size),
                       epochs = epochs,
                       verbose = 1
  )  
Sys.time()
keras::save_model_hdf5(model, filepath = "models/m_2_1_10_test.hdf5")

str(history)
history$params
history$metrics
plot(history)

# test the model
library(dplyr)
library(keras)

# load model
test_model <- keras::load_model_hdf5(filepath = "models/m_2_1_30_test.hdf5")

size <- c(45,60)
path.predict <- "Lores/Test"

generator.predict <- keras::flow_images_from_directory(
  directory = path.predict, 
  target_size = size,
  class_mode = NULL,
  batch_size = 1,
  shuffle = FALSE)

steps <- generator.predict$n

generator.predict$reset
predictions <- keras::predict_generator(test_model, 
                                        generator.predict, 
                                        steps = steps, verbose = 1) 

predictions[1,]

predm <- dim(predictions)[1]
pred.df <- data.frame(n=1:predm, class=-1)
for(i in 1:predm)
{
  p <- which(predictions[i,]==max(predictions[i,]))
  pred.df[pred.df$n==i,"class"] <- p
}

str(pred.df)

class.len <- length(generator.train$class_indices)
classes.df <- data.frame(name = names(generator.train$class_indices), i = 1:class.len)
str(classes.df)

fnames <- data.frame(n = seq(1,steps), name=generator.predict$filenames)
head(fnames)

fclass <- inner_join(fnames, pred.df, c("n"="n"))
fclassname <- inner_join(fclass, classes.df, c("class"="i")) %>% 
  mutate(fid = as.integer(substr(name.x, 3, nchar(name.x)-4)), 
         classid = class, classname = name.y) %>%
  select(fid, classid, classname)

str(fclassname)

source("Modules/dbData.R")
data <- db.data(db="AdventureWear",vw="vwStyles")

str(data)

inner_join(fclassname, data, c("fid"="id")) %>%
  filter(classname != category)

# test model: compare 3 models
library(dplyr)
library(keras)

# load model
test_model1 <- keras::load_model_hdf5(filepath = "models/m_2_1_10_test.hdf5")
test_model2 <- keras::load_model_hdf5(filepath = "models/m_2_1_20_test.hdf5")
test_model3 <- keras::load_model_hdf5(filepath = "models/m_2_1_30_test.hdf5")

size <- c(45,60)
path.predict <- "Lores/Test"

generator.predict <- keras::flow_images_from_directory(
  directory = path.predict, 
  target_size = size,
  class_mode = NULL,
  batch_size = 1,
  shuffle = FALSE)

steps <- generator.predict$n

fnames <- data.frame(n = seq(1,steps), name=generator.predict$filenames)

generator.predict$reset
predictions1 <- keras::predict_generator(test_model1, generator.predict, 
                                         steps = steps, verbose = 1) 

generator.predict$reset
predictions2 <- keras::predict_generator(test_model2, generator.predict, 
                                         steps = steps, verbose = 1) 

generator.predict$reset
predictions3 <- keras::predict_generator(test_model3, generator.predict, 
                                         steps = steps, verbose = 1) 

predm <- dim(predictions1)[1]

pred1.df <- data.frame(n=1:predm, class=-1)
for(i in 1:predm)
{
  p <- which(predictions1[i,]==max(predictions1[i,]))
  pred1.df[pred1.df$n==i,"class"] <- p
}

pred2.df <- data.frame(n=1:predm, class=-1)
for(i in 1:predm)
{
  p <- which(predictions2[i,]==max(predictions2[i,]))
  pred2.df[pred2.df$n==i,"class"] <- p
}

pred3.df <- data.frame(n=1:predm, class=-1)
for(i in 1:predm)
{
  p <- which(predictions3[i,]==max(predictions3[i,]))
  pred3.df[pred3.df$n==i,"class"] <- p
}

str(pred1.df)
pred.df <- inner_join(pred1.df, pred2.df, c("n"="n"), suffix=c(".1",".2"))
pred.df <- inner_join(pred.df, pred3.df, c("n"="n")) %>% 
  mutate(class.3=class)

pred.df %>% filter(class.1 != class.2 | class.1 != class.3 | class.2 != class.3)

# vote
for(i in 1:nrow(pred.df))
{
  c1 <- pred.df[i,"class.1"]
  c2 <- pred.df[i,"class.2"]
  c3 <- pred.df[i,"class.3"]
  
  if(c1==c2)
  {
    c <- c1
  } else if(c1==c3)
  {
    c <- c1
  } else
  {
    c <- c2
  }
  
  pred.df[i,"class"] <- c
}

slice_head(pred.df, n=10)
slice_tail(pred.df, n=10)

class.len <- length(generator.train$class_indices)
classes.df <- data.frame(name = names(generator.train$class_indices), i = 1:class.len)
str(classes.df)

fclass <- inner_join(fnames, pred.df, c("n"="n"))
fclassname <- inner_join(fclass, classes.df, c("class"="i")) %>% 
  mutate(fid = as.integer(substr(name.x, 3, nchar(name.x)-4)), 
         classid = class, classname = name.y) %>%
  select(fid, classid, classname)

str(fclassname)

source("Modules/dbData.R")
data <- db.data(db="AdventureWear",vw="vwStyles")

str(data)

inner_join(fclassname, data, c("fid"="id")) %>%
  filter(classname != category)

