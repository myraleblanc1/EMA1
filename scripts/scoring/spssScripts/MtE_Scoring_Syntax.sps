* Encoding: UTF-8.
DATASET ACTIVATE DataSet2.
COMPUTE Care_mean=Mean(Care1,Care2,Care3,Care4,Care5,Care6,Care7,Care8,Care9,Care10,Care11,Care12,
    Care13,Care14,Care15,Care16,Care17,Care18,Care19,Care20).
EXECUTE.

COMPUTE Protect_mean=Mean(Protect1,Protect2,Protect3,Protect4,Protect5,Protect6,Protect7,Protect8,
    Protect9,Protect10,Protect11,Protect12,Protect13,Protect14,Protect15,Protect16,Protect17,Protect18,
    Protect19,Protect20).
EXECUTE.

COMPUTE AffectsMe_mean=Mean(AffectsMe1,AffectsMe2,AffectsMe3,AffectsMe4,AffectsMe5,AffectsMe6,AffectsMe7,AffectsMe8,
    AffectsMe9,AffectsMe10,AffectsMe11,AffectsMe12,AffectsMe13,AffectsMe14,AffectsMe15,AffectsMe16,AffectsMe17,AffectsMe18,
    AffectsMe19,AffectsMe20).
EXECUTE.

COMPUTE Manipulate_mean=Mean(Manipulate1,Manipulate2,Manipulate3,Manipulate4,Manipulate5,Manipulate6,Manipulate7,Manipulate8,
    Manipulate9,Manipulate10,Manipulate11,Manipulate12,Manipulate13,Manipulate14,Manipulate15,Manipulate16,Manipulate17,Manipulate18,
    Manipulate19,Manipulate20).
EXECUTE.

COMPUTE Information_mean=Mean(Information1,Information2,Information3,Information4,Information5,Information6,Information7,Information8,
    Information9,Information10,Information11,Information12,Information13,Information14,Information15,Information16,Information17,Information18,
    Information19,Information20).
EXECUTE.

COMPUTE GetSomething_mean=Mean(GetSomething1,GetSomething2,GetSomething3,GetSomething4,GetSomething5,GetSomething6,GetSomething7,GetSomething8,
    GetSomething9,GetSomething10,GetSomething11,GetSomething12,GetSomething13,GetSomething14,GetSomething15,GetSomething16,GetSomething17,GetSomething18,
    GetSomething19,GetSomething20).
EXECUTE.

COMPUTE Virtuous_mean=Mean(Care_mean,Protect_mean,AffectsMe_mean).
EXECUTE.

COMPUTE Nonvirtuous_mean=Mean(Manipulate_mean,Information_mean,GetSomething_mean).
EXECUTE.

COMPUTE TotalMotives_mean=Mean(Virtuous_mean,Nonvirtuous_mean).
EXECUTE.
