import json
import networkx as nx
from networkx.algorithms import bipartite
import matplotlib.pyplot as plt
import operator
import re
import numpy as np
import pandas as pd
from collections import defaultdict
import csv
import random
import scipy.integrate

# course selection information of 1861 students
file=open("data_dump_1973.json")
file=json.load(file)

# create a network between students and courses (bipartite network)
G=nx.Graph()
student_index=list()
classes_name=list()
student_classes=list()
for i in file:
    if file[i]["Classes"]!=[]:
        student_index.append(i)
        for classes in file[i]["Classes"]:
            student_classes.append((i,classes))
            if classes in classes_name:
                continue
            classes_name.append(classes)
G.add_nodes_from(student_index,bipartite=0)
G.add_nodes_from(classes_name,bipartite=1)
G.add_edges_from(student_classes)

# projected bipartite network (students only)
P=bipartite.weighted_projected_graph(G,student_index)

visualization
classes_node_size=[3*G.degree(v) for v in classes_name]
nx.draw_networkx_nodes(G,nodelist=student_index, pos = nx.random_layout(G),with_labels=False, node_color = "yellow",node_shape="o", edge_color = "red",width=0.05,node_size =3,label="Students")
nx.draw_networkx_nodes(G,nodelist=classes_name, pos = nx.random_layout(G),with_labels=False, node_color = "blue",node_shape="*", edge_color = "red",width=0.05,node_size = classes_node_size,label="Classes")
plt.legend(loc="lower right",facecolor='grey')
nx.draw_networkx_edges(G, pos = nx.random_layout(G),with_labels=False, edge_color = "red",width=0.1,label="Student-Class")
plt.title("Bipartite graph of students and classes",size=20)
ax1=plt.axes()
ax1.set_facecolor("thistle")
plt.show()
nx.draw_networkx_nodes(P,pos = nx.random_layout(P),with_labels=False, node_color = "blue", edge_color = "red",width=0.01,node_size =8,label="Students")
plt.legend(loc="lower right",facecolor='grey')
nx.draw_networkx_edges(P, pos = nx.random_layout(P),with_labels=False, edge_color = "red",width=0.03,label="Student-Student")
plt.title("Projected bipartite graph of students",size=20)
ax2=plt.axes()
ax2.set_facecolor("lavender")
plt.show()

# dump important information
f=open("data.txt","w")

#If two students are connected in the projected bipartite network, how many same courses do they have?
print("1. If two students are connected in the projected bipartite network, how many same courses do they have?",file=f)
N_sameclasses=dict()
for i in list(P.edges(data=True)):
    weight=i[2]["weight"]
    if weight in N_sameclasses:
        N_sameclasses[weight]=N_sameclasses[weight]+1
    else:
        N_sameclasses[weight]=1
print("\nN_sameclasses:\n",N_sameclasses,file=f)
sum=sum(N_sameclasses.values())
N_sameclasses_prob=dict((k,v/sum) for k,v in N_sameclasses.items())
print("\nN_sameclasses_prob:\n",N_sameclasses_prob,file=f)

N_sameclasses_all=[i[2]["weight"] for i in list(P.edges(data=True))]
sameclasses_all=pd.DataFrame(N_sameclasses_all,columns=["number.of.same.classes"])
sameclasses_all.to_csv("Q1.csv")

# How many components can all 1861 students be divided into? (Every node in a component has a path
# to every other node and no other node has a path to any node in this component)
print("\n\n\n2. How many components can all 1861 students be divided into? \n(Every node in a component has a path to every other node and no other node has a path to any node in this component)",file=f)
P_components_number=nx.number_connected_components(P)
P_components=sorted(nx.connected_components(P))
P_components_size=list(len(i) for i in P_components)
print("\nP_components_number:\n",P_components_number,file=f)
print("\nP_components_size:\n",P_components_size,file=f)
components_df=pd.DataFrame(P_components_size,columns=["components_size"])
components_df.to_csv("Q2.csv")

# What are the shortest paths from one student to every other student in each component?
print("\n\n\n3. What are the shortest paths from one student to every other student in each component?",file=f)
N_steps_students=list()
N_steps_students_average=list()
for i in P_components:
    subgraph_distance=list()
    P_subgraph=P.subgraph(i).copy()
    all_pairs = dict(nx.all_pairs_shortest_path_length(P_subgraph))
    for node1 in all_pairs:
        for node2 in all_pairs[node1]:
            subgraph_distance.append(all_pairs[node1][node2])
    N_steps_students.append(subgraph_distance)
    N_steps_students_average.append(nx.average_shortest_path_length(P_subgraph))
N_steps_students_statistics=list()
for i in N_steps_students:
    subgraph=dict()
    for j in i:
        if j in subgraph:
            subgraph[j]=subgraph[j]+1
        else:
            subgraph[j]=1
    N_steps_students_statistics.append(subgraph)
print("\nN_steps_students_statistics:\n",N_steps_students_statistics,file=f)
print("\nN_steps_students_average:\n",N_steps_students_average,file=f)
N_steps_students_p1=N_steps_students[0]
N_steps_students_p1_df=pd.DataFrame(N_steps_students_p1,columns=["N_steps"])
N_steps_students_p1_df.to_csv("Q3.csv")


# Using three methods to assess the centrality of the selected courses.
print("\n\n\n4. Use three methods to assess the centrality of all the selected courses and then design a rectified value based on the three calculated values.",file=f)
degCent=nx.degree_centrality(G)
degCent_classes=dict((key,value) for key,value in degCent.items() if not re.match("\d+",key))
sorted_degCent=sorted(degCent_classes.items(),key=operator.itemgetter(1),reverse=True)

closeCent=nx.closeness_centrality(G,wf_improved=True)
closeCent_classes=dict((key,value) for key,value in closeCent.items() if not re.match("\d+",key))
sorted_closeCent=sorted(closeCent_classes.items(),key=operator.itemgetter(1),reverse=True)

btwnCent=nx.betweenness_centrality(G,normalized=True,endpoints=False)
btwnCent_classes=dict((key,value) for key,value in btwnCent.items() if not re.match("\d+",key))
sorted_btwnCent=sorted(btwnCent_classes.items(),key=operator.itemgetter(1),reverse=True)

pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
Cent1=pd.DataFrame(sorted_degCent,columns=["Classes","degCent"])
Cent2=pd.DataFrame(sorted_closeCent,columns=["Classes","closeCent"])
Cent3=pd.DataFrame(sorted_btwnCent,columns=["Classes","btwnCent"])
Cent12=pd.merge(Cent1,Cent2,on="Classes")
Cent=pd.merge(Cent12,Cent3,on="Classes")
Cent["rectifiedCent"]=1/3*(Cent["degCent"]-min(Cent["degCent"]))/(max(Cent["degCent"])-min(Cent["degCent"]))
+1/3*(Cent["closeCent"]-min(Cent["closeCent"]))/(max(Cent["closeCent"])-min(Cent["closeCent"]))
+1/3*(Cent["btwnCent"]-min(Cent["btwnCent"]))/(max(Cent["btwnCent"])-min(Cent["btwnCent"]))
Cent=Cent.sort_values(by="rectifiedCent",ascending=False)
Cent=Cent.set_index("Classes")
print("\n",Cent,file=f)
rectifiedCent=Cent["rectifiedCent"].tolist()
print("\n",rectifiedCent,file=f)

random.shuffle(rectifiedCent)

rectifiedCent_df=pd.DataFrame(rectifiedCent,columns=["rectified.centrality"])
rectifiedCent_df.to_csv("Q4.csv")


# How many students every other student is exposed to?
print("\n\n\n5. How many students every other student is exposed to?",file=f)
P_degree=[]
for i in P:
    P_degree.append(P.degree(i))
print("\nP_degree = \n", P_degree, file=f)
print("\nmax : \n",max(P_degree),file=f)
print("\nmean : \n",np.mean(P_degree),file=f)
P_degree_df=pd.DataFrame(P_degree,columns=["P_degree"])
P_degree_df.to_csv("Q5.csv")

# What's the number of students per class?
print("\n\n\n6. What's the number of students per class?",file=f)
classes_nodes={i for i,j in G.nodes(data=True) if j["bipartite"]==1}
classes_degree={}
for i in classes_nodes:
    classes_degree[i]=G.degree(i)
print("\nclasses degree : \n", classes_degree.values(),file=f)
print("\naverage of classes degree : \n",np.mean(list(classes_degree.values())),file=f)
classes_degree_df=pd.DataFrame(list(classes_degree.values()),columns=["classes_degree"])
classes_degree_df.to_csv("Q6.csv")

# What's the flow rate in one classroom on a given day?
print("\n\n\n7. What's the flow rate in one classroom on a given day?",file=f)
Rooms_Dates={}
Rooms_Dates=defaultdict(list)
for i in file:
    for j in file[i]["Rooms"]:
        if j != "Unknown" and j != "Online Course, Online Course, Room Online" and j !="Online Course, Online Course, Room SYNC":
            if file[i]["Times"][file[i]["Rooms"].index(j)][0]!="Arranged":
                Rooms_Dates[j].append(file[i]["Times"][file[i]["Rooms"].index(j)][0])
# print(Rooms_Dates)
Monday=[]
Tuesday=[]
Wednesday=[]
Thursday=[]
Friday=[]
for i in Rooms_Dates:
    Monday.append(re.findall("M",str(Rooms_Dates[i])))
    Tuesday.append(re.findall("T",str(Rooms_Dates[i])))
    Wednesday.append(re.findall("W",str(Rooms_Dates[i])))
    Thursday.append(re.findall("R",str(Rooms_Dates[i])))
    Friday.append(re.findall("F",str(Rooms_Dates[i])))
Monday_n=[]
Tuesday_n=[]
Wednesday_n=[]
Thursday_n=[]
Friday_n=[]
Rooms=[]
for i in range(0,len(Monday)):
    Monday_n.append(len(Monday[i]))
for i in range(0,len(Tuesday)):
    Tuesday_n.append(len(Tuesday[i]))
for i in range(0,len(Wednesday)):
    Wednesday_n.append(len(Wednesday[i]))
for i in range(0,len(Thursday)):
    Thursday_n.append(len(Thursday[i]))
for i in range(0,len(Friday)):
    Friday_n.append(len(Friday[i]))
for i in Rooms_Dates.keys():
    Rooms.append(i)
data_Rooms_Dates={"Rooms":Rooms,"Monday":Monday_n,"Tuesday":Tuesday_n,"Wednesday":Wednesday_n,
"Thursday":Thursday_n,"Friday":Friday_n}
df_Rooms_Dates=pd.DataFrame(data_Rooms_Dates)
df_Rooms_Dates=df_Rooms_Dates.set_index("Rooms")
# pd.set_option('display.max_rows', None)
# pd.set_option('display.max_columns', None)
print("\nDataframe(Rooms-Dates):\n",df_Rooms_Dates,file=f)
print("\nMaximum flow rate on Monday is in the classroom ",
df_Rooms_Dates["Monday"].idxmax(),"and the flow rate is ",max(df_Rooms_Dates["Monday"]),file=f)
print("\nMaximum flow rate on Tuesday is in the classroom ",
df_Rooms_Dates["Tuesday"].idxmax(),"and the flow rate is ",max(df_Rooms_Dates["Tuesday"]),file=f)
print("\nMaximum flow rate on Wednesday is in the classroom ",
df_Rooms_Dates["Wednesday"].idxmax(),"and the flow rate is ",max(df_Rooms_Dates["Wednesday"]),file=f)
print("\nMaximum flow rate on Thursday is in the classroom ",
df_Rooms_Dates["Thursday"].idxmax(),"and the flow rate is ",max(df_Rooms_Dates["Thursday"]),file=f)
print("\nMaximum flow rate on Friday is in the classroom ",
df_Rooms_Dates["Friday"].idxmax(),"and the flow rate is ",max(df_Rooms_Dates["Friday"]),file=f)
df_Rooms_Dates.to_csv("Q7.csv")

# What's the potential flow rate in the dining hall on a given day?
print("\n\n\n8. What's the potential flow rate in the dining hall on a given day?",file=f)
M={}
T={}
W={}
R={}
F={}
M=defaultdict(list)
T=defaultdict(list)
W=defaultdict(list)
R=defaultdict(list)
F=defaultdict(list)
for i in file:
    for j in file[i]["Times"]:
        if re.match(".*M.*",j[0]):
            M[i].append(j[1])
        if re.match(".*T.*",j[0]):
            T[i].append(j[1])
        if re.match(".*W.*",j[0]):
            W[i].append(j[1])
        if re.match(".*R.*",j[0]):
            R[i].append(j[1])
        if re.match(".*F.*",j[0]):
            F[i].append(j[1])
# print("\nThe class time for each student on work days:","\nMonday:\n",M,"\nTuesday:\n",T,
# "\nWednesday:\n",W,"\nThursday:\n",R,"\nFriday:\n",F,file=f)
Lunch_M={}
Lunch_T={}
Lunch_W={}
Lunch_R={}
Lunch_F={}
for i in M:
    if len(M[i])>1:
        if re.match(".*AM.*PM.*", str(M[i])) or re.match(".*PM.*AM.*", str(M[i])):
            Lunch_M[i]="Yes"
        else:
            Lunch_M[i]="No"
for i in T:
    if len(T[i])>1:
        if re.match(".*AM.*PM.*", str(T[i])) or re.match(".*PM.*AM.*", str(T[i])):
            Lunch_T[i]="Yes"
        else:
            Lunch_T[i]="No"
for i in W:
    if len(W[i])>1:
        if re.match(".*AM.*PM.*", str(W[i])) or re.match(".*PM.*AM.*", str(W[i])):
            Lunch_W[i]="Yes"
        else:
            Lunch_W[i]="No"
for i in R:
    if len(R[i])>1:
        if re.match(".*AM.*PM.*", str(R[i])) or re.match(".*PM.*AM.*", str(R[i])):
            Lunch_R[i]="Yes"
        else:
            Lunch_R[i]="No"
for i in F:
    if len(F[i])>1:
        if re.match(".*AM.*PM.*", str(F[i])) or re.match(".*PM.*AM.*", str(F[i])):
            Lunch_F[i]="Yes"
        else:
            Lunch_F[i]="No"
# print("\n\n\nExtra time to have lunch at school on work days (Yes or No):","\nMonday:\n",Lunch_M,
# "\nTuesday:\n",Lunch_T,"\nWednesday:\n",Lunch_W,"\nThursday:\n",Lunch_R,"\nFriday:\n",Lunch_F,file=f)
dininghall_flowrate={}
dininghall_flowrate["Monday"]=len([i for i in Lunch_M.values() if i=="Yes"])
dininghall_flowrate["Tuesday"]=len([i for i in Lunch_T.values() if i=="Yes"])
dininghall_flowrate["Wednesday"]=len([i for i in Lunch_W.values() if i=="Yes"])
dininghall_flowrate["Thursday"]=len([i for i in Lunch_R.values() if i=="Yes"])
dininghall_flowrate["Friday"]=len([i for i in Lunch_F.values() if i=="Yes"])
print("\n\n\n",dininghall_flowrate,file=f)
flowrate_df=pd.DataFrame.from_dict(dininghall_flowrate,orient='index',columns=["dininghall_flowrate"])
flowrate_df.to_csv("Q8.csv")

# How many students are connected directly and connected in total?
print("\n\n\n9. How many students are connected directly and connected in total?",file=f)
connect_directly=len(P.edges())
print("\nThe pair of students connecting directly:\n",connect_directly,file=f)
total=0
for i in P_components:
    length=len(i)
    total=total+length*(length-1)/2
print("\nThe pair of students connecting in total:\n",total,file=f)

# How to slow down the transmission by switching some classes to fully online?
# (Clustering coefficient measures the degree to which nodes in a network tend to form triangles)
print("\n\n\n10. How to slow down the transmission by switching some classes to fully online?\n(Clustering coefficient measures the degree to which nodes in a network tend to form triangles)",file=f)
clustering = nx.average_clustering(P)
G_test=G.copy()
G_test.remove_node("FT100/Lecture/1")
G_test.remove_node("FA102L/Lecture/1")
G_test.remove_node("BI242/Lecture/2")
G_test.remove_node("BI204/Lecture/2")
G_test.remove_node("CH201/Lecture/2")
P_test=bipartite.weighted_projected_graph(G_test,student_index)
print("\n Clustering before removing the high centrality class:\n",nx.average_clustering(P),file=f)
print("\n Clustering after removing the high centrality class:\n",nx.average_clustering(P_test),file=f)

# What are important nodes needing to be removed in order to cut the transmission?
print("\n\n\n11. What are the minimum nodes needing to be removed in order to cut the transmission?",file=f)
G_components=sorted(nx.connected_components(G))
node_cut=[]
for i in G_components:
    G_subgraph=G.subgraph(i).copy()
    node_cut.append(nx.minimum_node_cut(G_subgraph))
print("\n Minimum nodes needing to be removed in order to cut the transmission:\n",node_cut,file=f)


# SIR model
def SIR_model(y,t,N,beta,gamma):
    S,I,R= y
    dS_dt= -beta*S*I/N
    dI_dt= beta*S*I/N-gamma*I
    dR_dt= gamma*I
    return([dS_dt,dI_dt,dR_dt])

print("Enter the R0 value (0.0 ~ 5.7) for SIR_model:")
k=0
while k==0:
    try:
        R0_=float(input())
        if 0.0<=R0_ and R0_<=5.7:
            k=1
        else:
            print("R0 should be a value from 0.0 to 5.7")
            print("Enter the R0 value (0.0 ~ 5.7) for SIR_model:")
    except:
        print("R0 should be a value from 0.0 to 5.7")
        print("Enter the R0 value (0.0 ~ 5.7) for SIR_model:")
N=1861
I0=1861*688/100000
R0=0
S0=N-I0-R0
gamma=1/7   ##mean recovery rate (1/recovery period)
beta=R0_*gamma  ##infectious rate

t= np.linspace(0,80,80)
result1= scipy.integrate.odeint(SIR_model,[S0,I0,R0],t,args=(N,beta,gamma))
result1=np.array(result1)

plt.figure()
plt.subplot(121)
plt.plot(t,result1[:,0],label="Susceptible")
plt.plot(t,result1[:,1],label="Infectious")
plt.plot(t,result1[:,2],label="Recovered")
plt.vlines(15,-50,1800,colors="c",linestyles = "dashed",label="last day of fall quarter")
plt.legend(loc="best")
plt.grid()
plt.xlabel("week")
plt.ylabel("student numbers")
plt.title("SIR model simulation\n (The chosen R0 value is {})".format(R0_))

# SEIR model
def SEIR_model(y,t,N,beta,sigma,gamma):
    S,E,I,R= y
    dS_dt= -beta*S*I/N
    dE_dt= beta*S*I/N-sigma*E
    dI_dt= sigma*E-gamma*I
    dR_dt= gamma*I
    return([dS_dt,dE_dt,dI_dt,dR_dt])

print("Enter the R0 value (0.0 ~ 5.7) for SEIR_model:")
k=0
while k==0:
    try:
        R0_=float(input())
        if 0.0<=R0_ and R0_<=5.7:
            k=1
        else:
            print("R0 should be a value from 0.0 to 5.7")
            print("Enter the R0 value (0.0 ~ 5.7) for SEIR_model:")
    except:
        print("R0 should be a value from 0.0 to 5.7")
        print("Enter the R0 value (0.0 ~ 5.7) for SEIR_model:")

N=1861
E0=0
I0=1861*688/100000
R0=0
S0=N-I0-E0-R0
sigma=1/5.2*7 #incubation rate (1/latent period)
gamma=1/7   ##mean recovery rate (1/recovery period)
beta=R0_*gamma  ##infectious rate

t= np.linspace(0,80,80)
result2= scipy.integrate.odeint(SEIR_model,[S0,E0,I0,R0],t,args=(N,beta,sigma,gamma))
result2=np.array(result2)
plt.subplot(122)
plt.plot(t,result2[:,0],label="Susceptible")
plt.plot(t,result2[:,2],label="Infectious")
plt.plot(t,result2[:,3],label="Recovered")
plt.plot(t,result2[:,1],label="Exposed")
plt.vlines(15,-50,1800,colors="c",linestyles = "dashed",label="last day of fall quarter")
plt.legend(loc="best")
plt.grid()
plt.xlabel("week")
plt.ylabel("student numbers")
plt.title("SEIR model simulation\n (The chosen R0 value is {})".format(R0_))
plt.show()
