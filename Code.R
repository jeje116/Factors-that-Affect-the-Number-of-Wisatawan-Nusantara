library(readxl)
Dataset_Paper <- read_excel(file.choose())
#Uji Chow
#H0 : Model Common Effect
#H1 : Model Fixed Effect

library(plm)
common=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Budaya + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="pooling")

fixed=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
            Daya_Tarik_Wisata_Budaya + 
            Daya_Tarik_Wisata_Buatan + 
            Taman_Hiburan_dan_Rekreasi + 
            Kawasan_Pariwisata + 
            Wisata_Tirta,data=Dataset_Paper,model="within")

pooltest(common,fixed)
#p-value = 0.01805 < 0.05
#tolak H0, kesimpulan memakai model Fixed Effect


#Ujji Hausman
# membuat model regresi panel

#H0 : Model Random Effect
#H1 : Model Fixed Effect

fixed=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
            Daya_Tarik_Wisata_Budaya + 
            Daya_Tarik_Wisata_Buatan + 
            Taman_Hiburan_dan_Rekreasi + 
            Kawasan_Pariwisata + 
            Wisata_Tirta,data=Dataset_Paper,model="within",index = c("Provinsi","Tahun"))

random=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Budaya + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",index = c("Provinsi","Tahun"))

phtest(fixed,random)

#p-value = 0.3595 > 0.05
#gagal tolak H0, kesimpulan memakai model Random Effect


#Uji Breusch Pagan
gr=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
         Daya_Tarik_Wisata_Budaya + 
         Daya_Tarik_Wisata_Buatan + 
         Taman_Hiburan_dan_Rekreasi + 
         Kawasan_Pariwisata + 
         Wisata_Tirta,data=Dataset_Paper,model="random")

#Ketika nilai p-value kurang dari alpha maka dapat disimpulkan ada efek.

#Efek Dua Arah
plmtest(gr, effect="twoways", type="bp")

#p-value = 0.08022 > 0.05
#tidak ada efek

#Efek Individu/Cross Section
plmtest(gr, effect="individual", type="bp")

#p-value = 0.03603
#karena p-value kurang dari alpha maka dapat disimpulkan ada efek Cross Section

#Efek Waktu/Time
plmtest(gr, effect="time", type="bp")

#p-value = 0.4201
#tidak ada efek

#Pemilihan model
model1=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Budaya + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))

summary(model1)

model2=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))

summary(model2)

model3=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Buatan + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))

summary(model3)

pbgtest(model1)
pbgtest(model2)
pbgtest(model3)

library(lmtest)
bptest(model1)
bptest(model2)
bptest(model3)

model=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
            Daya_Tarik_Wisata_Buatan + 
            Kawasan_Pariwisata + 
            Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))

#H0 : Error memiliki variance konstan (homoskedastis)
#H1 : Error tidak memiliki variance konstan (heterokedastis)

bptest(model)

#p-value = 0.006074
#tolak H0, kesimpulan error heterokedastis

#H0 : Error tidak berautokorelasi
#H1 : Error berautokorelasi
#dwtest(model)
pbgtest(model)

#p-value = 0.06209
#gagal tolak H0, kesimpulan error berautokorelasi

library(car)
vif(model)

#tidak ada multicolinearity

#WLs
model.fitted <- model$model[[1]] - model$residuals
weight = 1 / lm(abs(model$residuals) ~ model.fitted)$fitted^2
WLSModel = plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
                 Daya_Tarik_Wisata_Buatan + 
                 Kawasan_Pariwisata + 
                 Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"),weights = weight)

summary(WLSModel)

bptest(WLSModel)
pbgtest(WLSModel)
vif(WLSModel)

#Melihat seberapa besar pengaruh masing-masing cross section
ranef(WLSModel)
