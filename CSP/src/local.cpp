#include <vector>
#include <string>
#include "assert.h"
#include <random>
#include <algorithm>
#include <iostream>

using namespace std;
using matrix = vector<vector<bool>>;
struct DNA{
    DNA() = default;
    explicit DNA(int length){
        randomGen(length);
    }
    inline int size(){return code.size();}
    vector<bool> code;
    matrix decode(int rowlength,int cols){
        matrix temp(cols,vector<bool>(rowlength,false));
        for(int i=0;i<cols;i++){
            for(int j =0 ;j<rowlength;j++){
                temp[i][j]=code[i*rowlength+j];
            }
        }
        return temp;
    }
    void randomGen(int totallength){
        for(int i=0;i<totallength;i++){
            code.push_back(rand()%2==0);
        }
    }
};

class GA{
   public:
    GA() = default;
    GA(int week,int works){weekdays = week,workers = works;};
    void randomInit(int amount){
        assert(amount>2);
        parents.clear();
        for(int i=0;i<amount;i++){
            parents.push_back(DNA(weekdays*workers));
        }
        n = amount;
        elite = amount*0.2+1;//
        time = 0;
    }
    void cross(){
        vector<DNA> next;
        for(int i=0;i<n;i++){
            //每一个后代
            int father = rand()%elite;
            int mother = (father + 1)%elite;
            DNA child = parents[father];
            int pos = rand()%(parents[father].size());
            //杂交位点
            copy(parents[mother].code.begin(),
            parents[mother].code.begin()+pos,
            child.code.begin());
            next.push_back(child);
        }
        parents = next;
    }//parents替换成后代
    void mutate(){
        //possibility = possibility*pow(2,-(time*0.1));
        if(rand()%1000 <= 1000*possibility){
            int pos = rand()%(parents[0].size());
            int id = rand()%n;
            parents[id].code[pos] = !parents[id].code[pos];
        }
    }
    //改变parents. 选择amount*0.2个最好的(punishment最低).
    void select(){
        std::partial_sort(parents.begin(),parents.begin()+elite,parents.end(),[=](const DNA& lhs,const DNA& rhs){
            return this->punishment(lhs) < this->punishment(rhs);
        });
    }
    virtual int punishment(DNA p){
        auto M = p.decode(workers,weekdays);
        //每个人检查其违反的约束条目
        //每天检查其违反的约束条目
        //返回加起来的总和.

        int punish = 0;
        int workercount = 0;
        for(int i=0;i<weekdays;i++){
            workercount = 0;
            int sencount = 0;
            for(int j=0;j<workers;j++){
                workercount += M[i][j];
                if(M[i][j]&&(j==0||j==1))
                    sencount ++;
            }
            if(M[i][1-1]&&M[i][4-1])
                punish++;
            if(M[i][2-1]&&M[i][3-1])
                punish++;
            if(M[i][3-1]&&M[i][6-1])
                punish++;
            if(workercount<4) punish += (4-workercount)/2;
            if(sencount<1) punish++;
        }
        for(int j=0;j<workers;j++){
            int restday = 0;
            int state = 0;
            for(int i=0;i<weekdays;i++){
                if(M[i][j])
                    state = 0;
                else{
                    if(++state>=3){
                        punish+=3;
                    }
                    restday++;
                }
            }
            if(restday<2){
                punish += 2-restday;
            }
        }
        return punish;
    }
    virtual matrix run(int iters,int amount){
        randomInit(amount);
        int count = 0;//防止死循环。
        int staying = 0;
        int last = punishment(parents[0]);
        int st = 0;
        while(last!=0 && count++ < iters){
            mutate();
            select();
            cross();
            time ++;
            if(st==last){
                if(++staying==50){
                    int pos = rand()%(parents[0].size());
                    parents[0].code[pos] = !parents[0].code[pos];
                    cross();//强制变异,杂交保留
                    count++;
                    time++;
                }
            }
            else
                staying = 0;
            st = last;
            last = punishment(parents[0]);
            
        }
        return parents[0].decode(workers,weekdays);
    }
    vector<DNA> parents;
    double possibility = 0.45;
    int n;
    int workers;
    int weekdays=7;
    int elite;
    int time = 0;
};

class GA2:public GA{
    int atsame;
   public:
    GA2(int wd,int ws,int ats){
        weekdays = wd;
        workers = ws;
        atsame = ats;
        possibility = 0.85;
    }
    virtual int punishment(DNA p) final{
        auto M = p.decode(workers,weekdays);
        //每个人检查其违反的约束条目
        //每天检查其违反的约束条目
        //返回加起来的总和.

        int punish = 0;
        int workercount = 0;
        for(int i=0;i<weekdays;i++){
            workercount = 0;
            int sencount = 0;
            for(int j=0;j<workers;j++){
                workercount += M[i][j];
                if(M[i][j]&&(j==0||j==1||j==7||j==9))
                    sencount ++;
            }
            if(M[i][1-1]&&M[i][5-1])
                punish++;
            if(M[i][2-1]&&M[i][6-1])
                punish++;
            if(M[i][8-1]&&M[i][10-1])
                punish++;
            if(workercount<atsame){punish++;}
            if(sencount<1) punish++;
        }
        for(int j=0;j<workers;j++){
            int restday = 0;
            int state = 0;
            for(int i=0;i<weekdays;i++){
                if(M[i][j])
                    state = 0;
                else{
                    if(++state>=3)
                        punish++;
                    restday++;
                }
            }
            if(restday<2){
                punish ++;
            }
        }
        return punish;
    }
};



#include<chrono>

void test1(void){
    GA G(7,7);
    srand(1115);
    G.possibility = 0.9;
    auto begin  = std::chrono::steady_clock::now();
    auto ans = G.run(10000,7);
    auto end = std::chrono::steady_clock::now();
    for(int i=0;i<7;i++){
        for(int j=0;j<7;j++){
            cout << ans[i][j] << "\t";
        }
        cout << endl;
    }
    //cout << G.punishment(G.parents[0]) << endl;
    cout << "generations= " << G.time <<endl;
    cout << "GA1time=" << double((end-begin).count()/1000)/1000000 << endl;
}

void test2(void){
    GA2 G(7,10,5);
    srand(119);
    auto begin  = std::chrono::steady_clock::now();
    auto ans = G.run(10000,7);
    auto end = std::chrono::steady_clock::now();
    for(int i=0;i<7;i++){
        for(int j=0;j<10;j++){
            cout << ans[i][j] << "\t";
        }
        cout << endl;
    }

    //cout << G.punishment(G.parents[0]) << endl;
    cout << "generations= " << G.time <<endl;
    cout << "GA2time=" << double((end-begin).count()/1000)/1000000 << endl;
}
int main(void){
    test1();
    test2();
}