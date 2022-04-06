library("audio")
library("tuneR")
library("seewave")

s1<-sin(2*pi*440*seq(0,1,length.out=8000))
oscillo(s1,f=8000)

savewav(s1, f=8000, filename = "s1.wav")

listen(s1, f=8000, from=0.0, to=1.0)

s2<-sin(2*pi*440*3*seq(0,1,length.out=3*8000))
oscillo(s2, f=8000)

savewav(s2, f=8000, filename = "s2.wav")

listen(s2, f=9000, from=0.0, to=3)





s3<-sin(2*pi*seq(0,500,length.out=1000))
oscillo(s3,f=1000)

savewav(s3, f=1000, filename = "s3.wav")

listen(s3, f=1000, from=0.0, to=1.0)




s4<-sin(2*pi*seq(0,300,length.out=1000))
oscillo(s4,f=1000)

savewav(s4, f=1000, filename = "s4.wav")

listen(s4, f=1000, from=0.0, to=1.0)






s5<- c(s3, s4)

oscillo(s5,f=1000)

savewav(s5, f=1000, filename = "s5.wav")

listen(s5, f=1000, from=0.0, to=2.0)






s6<-sin(2*pi*runif(1000,0,500))
oscillo(s6,f=1000)

savewav(s6, f=1000, filename = "s6.wav")

listen(s6, f=1000, from=0.0, to=1.0)






s7<-sin(2*pi*261.63*runif(1000,0,1))
oscillo(s7,f=1000)

savewav(s7, f=1000, filename = "s7.wav")

listen(s7, f=1000, from=0.0, to=1.0)







# White noise?
s8<-sin(2*pi*runif(10000,20,20000))
oscillo(s8,f=1000)

savewav(s8, f=1000, filename = "s8.wav")

listen(s8, f=1000, from=0.0, to=10.0)









noisew<-noisew(f=440, d=3.0, type="gaussian", listen = T)
oscillo(noisew,f=440)










# Example: Add noise to a synthetic signal
a<-noisew(d=1,f=8000)
b<-synth(f=8000,d=1,cf=2000,plot=F)
c<-a+b
listen(c,f=8000)











s9<-noisew(d=3,f=4000)
oscillo(s9[,1],f=4000)
listen(s9,f=4000)




s10<-noisew(d=3,f=10000)
oscillo(s10[,1],f=10000)
listen(s10,f=10000)





listen(c(s9,s10),f=10000)


oscillo(c(s9[,1],s10[,1]),f=10000)










# YES
# https://stackoverflow.com/questions/8697567/how-to-simulate-pink-noise-in-r

w <- noise(kind = c("white"))
p <- noise(kind = c("pink"))
par(mfrow=c(2,1))
plot(w,main="white noise")
plot(p,main="pink noise")
par(mfrow=c(1,1))

whiteNoise<- w@left
pinkNoise<- p@left

oscillo(whiteNoise,f=44100)
listen(whiteNoise,f=44100)


oscillo(pinkNoise,f=44100)
listen(pinkNoise,f=44100)

# Just out of interest, how would one write a generalized "color" noise function, i.e. suppress arbitrary regions of the bandwidth? That might be an enjoyable New Year's project for some R-nerd out there

# You generate white gaussian noise, then run the samples through a filter to generate the desired power spectrum. Pink noise is defined as one with "1/f" power spectrum, so you need to design a filter with a "1/sqrt(f)" frequency response. Usually, you design a FIR (finite impulse response) filter approximating the desired response in some frequency band of interest.



# BEST SO FAR

start.time <- Sys.time()

duration = 600

noise1 <- noise(kind = "white",
                duration = duration, 
                samp.rate = 44100,
                bit = 1,
                stereo = FALSE, 
                xunit = "time")

# plot(noise1,main="white noise")

# noise1vec<- noise1@left

# oscillo(noise1vec,f=44100, from=0.0, to=duration)
# listen(noise1vec,f=44100, from=0.0, to=duration)
# savewav(noise1vec, f=44100, filename = "noise1vec.wav")

savewav(noise1@left, f=44100, filename = "noise1vec.wav")

end.time <- Sys.time()

time.taken <- round(end.time - start.time,2)
time.taken

wav2flac("noise1vec.wav", overwrite=TRUE, path2exe = "C:/Users/ethan/Downloads/flac-1.3.4-win/flac-1.3.4-win/win64")










# Attempt at Brownian/red noise
duration = 600

noise2 <- noise(kind = "power",
                duration = duration, 
                samp.rate = 44100,
                bit = 1,
                stereo = FALSE, 
                xunit = "time",
                alpha = 2)

oscillo(noise2@left,f=44100, from=0.0, to=duration)

savewav(noise2@left, f=44100, filename = "noise2vec.wav")











# Attempt at blue noise
duration = 600

noise3 <- noise(kind = "power",
                duration = duration, 
                samp.rate = 44100,
                bit = 1,
                stereo = FALSE, 
                xunit = "time",
                alpha = -1)

noise3vec <- noise3@left

oscillo(noise3vec,f=44100, from=0.0, to=duration, fastdisp = T)

savewav(noise3vec, f=44100, filename = "noise3vec.wav")











# Attempt at violet noise
duration = 600

noise4 <- noise(kind = "power",
                duration = duration, 
                samp.rate = 44100,
                bit = 1,
                stereo = FALSE, 
                xunit = "time",
                alpha = -2)

noise4vec <- noise4@left

oscillo(noise4vec,f=44100, from=0.0, to=duration, fastdisp = T)

savewav(noise4vec, f=44100, filename = "noise4vec.wav")















# Attempt at black noise
duration = 600

noise5 <- noise(kind = "power",
                duration = duration, 
                samp.rate = 44100,
                bit = 1,
                stereo = FALSE, 
                xunit = "time",
                alpha = 3)

noise5vec <- noise5@left

oscillo(noise5vec,f=44100, from=0.0, to=duration, fastdisp = T)

savewav(noise5vec, f=44100, filename = "noise5vec.wav")












# Attempt at green noise
duration = 600

noise6 <- noise(kind = "power",
                duration = duration, 
                samp.rate = 44100,
                bit = 1,
                stereo = FALSE, 
                xunit = "time",
                alpha = 0)

noise6green <- ffilter(noise6, 
                       from = 1568, 
                       to = 2960, 
                       bandpass = TRUE,
                       output="Wave")

noise6vec <- noise6green@left
noise6vec <- noise6vec * 1e10

oscillo(noise6vec,f=44100, from=0.0, fastdisp = T)

savewav(noise6vec, f=44100, filename = "noise6vec.wav")


















colour = "black"
  
if (colour == "white") {
  alpha = 0
} else if (colour == "pink") {
  alpha = 1
} else if ((colour == "brown") | (colour == "red")) {
  alpha = 2
} else if (colour == "blue") {
  alpha = -1
} else if (colour == "violet") {
  alpha = -2
} else if (colour == "black") {
  alpha = 3
}

alpha















