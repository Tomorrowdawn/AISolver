#include <vector>
#include <list>
#include <map>
#include <initializer_list>
#include <string>
#include <assert.h>
#include <unordered_set>
#include <iostream>
#include <numeric>
#include <algorithm>

using namespace std;

namespace First{

using ID = int;
enum level{
    senior,junior
};

enum type{
    date,worker,NIL
};
struct Value{
    int ok = -1;//1 = yes,0 = no, -1 = fail
    level t = junior;
};


using Domain = list<Value>;


class VarList{
   public:
    VarList() = default;
    VarList(const vector<ID>& d){
        ids = d;
    }
    VarList(ID d){
        ids.push_back(d);
    }
    VarList(ID d1,ID d2){
        ids.push_back(d1);
        ids.push_back(d2);
    }
    std::string print(){
        string ans = "";
        for(auto id:ids){
            ans+= to_string(id) + ",";
        }
        return ans;
    }
    inline int size(){return ids.size();}
    inline auto begin(){return ids.begin();}
    inline auto end(){return ids.end();}
    inline bool empty(){return ids.empty();}
    //inline auto pop_back(){auto x = ids.back();ids.pop_back();return x;}
    inline auto pop(){
        if(ids.empty()){
            return ID();
        }
        auto id = ids.back();
        ids.pop_back();
        return id;
    }
    vector<ID> ids;
    //Use Var->getxxx(id) to access.
};

class Vars{
   public:
    Vars() = default;
    Vars(VarList ini){
        va = ini;
    }

    //vector<ID> ids;
    VarList va;
};

struct term{
    term() = default;
    term(VarList all){
        Value NIL;
        for(auto x:all){
            assignment[x] = NIL;
        }
    }
    term(VarList all,vector<ID> ids,vector<Value> vals){
        Value NIL;
        for(auto x:all){
            assignment[x] = NIL;
        }
        for(int i =0;i<ids.size();i++){
            assignment[ids[i]] = vals[i];
        }
    }
    term(ID id,Value e){
        assignment[id] = e;
    }
    VarList unassigned(VarList& pattern){
        vector<ID> result;
        for(auto p:pattern){
            if(assignment.find(p)==assignment.end()){//没找着
                result.push_back(p);
            }
        }
        return VarList(result);
    }
    inline int sum(){
        int s = 0;
        for(auto x:assignment){
            s += x.second.ok;
        }
        return s;
    }
    term assigned(VarList& pattern){
        term temp;
        for(auto p:pattern){
            if(assignment.find(p)!=assignment.end()){
                temp = temp + term(p,assignment[p]);
            }
        }
        return temp;
    }
    term operator+(const term& rhs)const{
        term Y = rhs;
        Y.assignment.insert(this->assignment.begin(),this->assignment.end());
        return Y;
    }
    Value& operator[](ID i){
        return assignment[i];
    }
    inline bool empty(){
        return assignment.empty();
    }
    VarList get_list(){
        vector<ID> rec;
        for(auto x:assignment){
            rec.push_back(x.first);
        }
        return VarList(rec);
    }
    term fill(term X)const{
        term Y = *this;
        for(auto x:X.assignment){
            Y.assignment[x.first] = x.second;
        }
        return Y;
    }
    Value get(ID id){
        if(assignment.find(id)==assignment.end()){
            Value k;
            k.ok = -1;
            return k;
        }
        return assignment[id];
    }
    void set(ID id,Value v){
        assignment[id] = v;
    }
    bool equals() const {
        if(assignment.size()!=2){
            return false;
        }
        else{
            auto a = assignment.begin();
            int a1  = (*a).second.ok;
            a++;
            int a2 = (*a).second.ok;
            return (a1==1&&a2==1);
        }
    }
    std::string print(int rows,int cols){
        string res = "";
        for(int i=0;i<rows;i++){
            //every day
            for(int j=0;j<cols;j++){
                int id = i*cols+j;
                if(assignment[id].ok==1){
                    res+= to_string(j+1) + " ";
                }
            }
            res +='\n';
        }
        return res;
    }
    map<ID,Value> assignment;
};


class Domains{
    
    bool failure = false;
   public:
    map<ID,Domain> D;
    Domains(const map<ID,Domain>& Ini){
        D = Ini;
    }
    Domains erase(ID id){
        auto temp = D;
        temp.erase(id);
        return Domains(temp);
    }
    Domains remove(ID id,Value r){
        auto temp = D;
        for(auto i = temp[id].begin();i!=temp[id].end();i++){
            if((*i).ok == r.ok){
                temp[id].erase(i);
                break;
            }
        }
        return Domains(temp);
    }
    void set(ID id,int val){
        bool flag = false;
        for(auto i=D[id].begin();i!=D[id].end();i++){
            if((*i).ok == val){
                flag = true;
            }
        }
        if(flag = true){
            D[id].clear();
            Value temp;
            temp.ok = val;
            D[id].push_back(temp);
        }
        else
            set_fail();
    }
    Domains pop_term(term& x){
        map<ID,Domain> temp = D;
        for(auto d:D){
            if(d.second.size()==1){
                x=x+term(d.first,d.second.back());
                temp.erase(d.first);
            }
        }
        return Domains(temp);
    }
    Domain rollup(ID id){
        return D[id];
    }
    inline void set_fail(){
        failure = true;
    }
    inline bool is_fail(){
        return failure;
    }
    inline bool empty(){
        return D.empty();
    }
};
class Solver{
   public:
    Solver(int labours){
        vector<ID> ids;
        for(int i=0;i<weekdays;i++){
            for(int j=0;j<labours;j++){
                ids.push_back(i*labours+j);
            }
        }
        workers = labours;
        Base = Vars(std::move(ids));
    }
    Solver(VarList All){
        Base = Vars(std::move(All));
    };
    void set_part_rel(const vector<pair<int,int>>& C){
        //注意外来的符号都是从1开始的
        map<ID,vector<VarList>> temp;
        for(auto t:C){
            for(int i=0;i<weekdays;i++){
                //每一天
                ID id1 = t.first - 1 + i*workers;
                ID id2 = t.second -1 + i*workers;
                temp[id1].push_back(VarList(id1,id2));
                temp[id2].push_back(VarList(id1,id2));
            }
        }
       /* for(auto x:temp){
            cerr << x.first << "=";
            for(auto y:x.second){
                cerr << "(" << y.print() << ")" <<",";
            }
            cerr << endl;
        }*/
        part_constraint = temp;
        rel_seted = true;
    }
    void set_worker_levels(vector<level> levels){
        for(int i=0;i<levels.size();i++){
            worker_levels[i] = levels[i];
            if(levels[i] == senior){
                sens.insert(i);
            }
        }
        work_seted = true;
    }
    void config(int ats,int seniors){
        atsameday = ats;
        sen = seniors;
        configed = true;
    }
    void test(){
        assert(this->rel_seted);
        assert(this->work_seted);
        assert(this->configed);
    }
    inline int getWorkOrder(ID id){
        return id % workers;
    }
    inline int getweekday(ID id){
        return id/ workers;
    }
    //T与pattern并不完全契合,需要先求出已赋值部分.
    //rel保证:如果T已经犯错了必然返回false(无需构造完). 如果还没犯错就返回true.
    virtual bool rel(VarList& pattern,term& T){
        //cout << "OK" << endl;
        auto as = T.assigned(pattern);
        return !as.equals();//只有不愿一起工作的。相等就错。
    }

    //check检查新加入的这个var=val和history冲不冲突. 如果当前已经冲突了就会立刻指出.
    //全局和局部
    bool check(term& history,ID var,Value val){
        auto cons = get_arc(var);
        for(auto con:cons){
            auto res = history+term(var,val);
            if(rel(con,res)==false)
                return false;
        }
        int weekday = var / workers;
        int worker = var % workers;
        //Varlist: (weekday,every work),(everyday,worker),(specific constraint for certain weekday or worker).
        int workcount = workers;
        int seniorcount = sen;//假设都是
        for(int i=0;i<workers;i++){
            //今天统计每人
            auto read =  history.get(i+weekday*workers);//id = weekday*workers+worker
            if(read.ok == 0){
                workcount --;
                seniorcount -= (read.t==senior);
            }
        }
        
        if(workcount < atsameday || seniorcount < 1){
            //cerr << history.print() << endl;
            //if(var==2&& val.ok ==0) abort();
           //cerr << workcount << "||" << seniorcount << endl;
            //cerr << "死了啦都是你害的啦" <<endl;
            return false;
        }
        int restcount = weekdays;
        int state = 0;
        for(int i=0;i<weekdays;i++){
            //此人每天
            auto read = history.get(i*workers+worker);
            if(read.ok==0){
                if(++state>=restlimit) return false;
            }
            else if(read.ok==1){
                restcount--;
                state = 0;
            }
        }
        if(restcount < restensure){
            return false;
        }
        return true;
    }
    //这个函数直接返回所有枚举,不会考虑合法性.
   /* vector<term> E(VarList X){
        vector<term> enumerate;
        if(X.empty()){
            return enumerate;
        }
        auto first = X.pop();
        auto last = E(X);
        auto i = Base.getall(first);
        if(last.empty()){
            for(auto x:i){
                enumerate.push_back(term(first,x));
            }
            return enumerate;
        }
        for(auto x:i){
            for(auto y:last){
                auto temp = term(first,x);
                enumerate.push_back(temp+y);
            }
        }
        return enumerate;
    }*/
    /*const vector<VarList>& getarcs()const{
        return inner_constraint;
    }
    //通用算法,要求constraint已被枚举出来.有时constraint太多,应当使用专用算法
    vector<VarList> get_arc(ID id){
        vector<VarList> ans;
        for(auto x:inner_constraint){
            if(x.ids.find(id)!=x.ids.end()){
                ans.push_back(x);
            }
        }
        return ans;
    }*/
    //基于约束结构而非枚举返回. 在特定问题上使用这个
    vector<VarList> get_arc_by_constraint(ID id){
        auto result = globalConstraint(id);
        result.insert(result.end(),part_constraint[id].begin(),part_constraint[id].end());
        return result;
    }
    //只返回二元约束
    vector<VarList> get_arc(ID id){
        return part_constraint[id];
    }
    vector<VarList> globalConstraint(ID id){
        int weekday = id / weekdays;
        int worker = id % weekdays;
        //接下来,构造所有与此天有关的weekday,以及worker. 直接返回并集的vector.
        //Varlist: (weekday,every work),(everyday,worker),(specific constraint for certain weekday or worker).
        vector<VarList> result;
        vector<ID> daysforworker(1,worker);
        vector<ID> workersforday(1,weekday);
        for(int i=0;i<workers;i++){
            workersforday.push_back(i+weekday*workers);//id = weekday*workers+worker
        }
        result.push_back(VarList(std::move(workersforday)));
        for(int i=0;i<weekdays;i++){
            daysforworker.push_back(i*workers+worker);
        }
        result.push_back(VarList(std::move(daysforworker)));
        return result;
    }
    std::string CSP(){
        auto solution = Backtrack(term(),ConstructStart());
        //TODO:
        return solution.print(weekdays,workers);
    }
    int count =0;
    term Backtrack(term A,Domains D){
        if(D.empty())//赋完了
            return A;
        auto var = fetch(D.D);
        auto Dom = D.rollup(var);
        auto rest = D.erase(var);
        for(auto value:Dom){
            count ++;
            //cout << "ID=" << var << "Val=" << value.ok <<endl;
            auto detect = A + term(var,value);
            if(check(detect,var,value)){
                //cout << "OK" << endl;
                if(infered){
                auto rDom = Infer(detect,var,value,rest);
                //auto rDom = detect;
                if(!rDom.is_fail()){
                    //term infer_term;rDom = rDom.pop_term(infer_term);
                    //auto res = detect+infer_term;
                    auto new_term = Backtrack(detect,rDom);
                    if(!new_term.empty())//成功
                        return new_term;
                }
                }else{
                //if(var == 2 && value.ok == 0) abort();
                auto new_term = Backtrack(detect,rest);
                if(!new_term.empty()){
                    return new_term;
                }
                }
            }
        }
        //cerr << "failto ID=" << var << endl;
        return term();//失败
    }
    Domains Infer(term history,ID var,Value val,Domains Dom){
        auto cons = get_arc(var);
        //对于每个con,查看history里面是否已经赋值;没有赋值,则进入Dom进行枚举.
        //每新生成一个term,送入rel进行检查.注意此时term不再是完全枚举,而是con指定的变量枚举
        for(auto con:cons){
            //cerr << "OK" << endl;
            auto un = history.unassigned(con);
            //cerr << "OK" << endl;
            for(auto u:un){
                bool flag = false;
                auto roll = Dom.rollup(u);
                for(auto v:roll){
                    auto res = term(u,v)+history;
                    //cerr <<"ID="<< u <<"val=" << v.ok << endl;
                    if(rel(con,res)==false){
                        Dom = Dom.remove(u,v);
                    }
                    else 
                        flag = true;
                }
                if(flag == false){
                    Dom.set_fail();
                    return Dom;
                }
            }
        }
        //全局约束
        //senior约束
        int workerid = getWorkOrder(var);
        int weekday = getweekday(var);
        if(sens.find(workerid)!=sens.end()){
            //senior,而且不上班
            if(val.ok==0){
                VarList today(getsens(weekday));
                auto un = history.unassigned(today);
                if(un.size()==1){
                    auto u = un.pop();
                    auto as = history.assigned(today);
                    int s = history.sum();
                    if(s<1){
                        Dom.set(u,1);
                        if(Dom.is_fail())//寄了
                            return Dom;
                    }
                }
            }
        }
        return Dom;
    }

    vector<ID> gettoday(int weekday){
        vector<ID> today;
        for(int i=0;i<workers;i++){
            today.push_back(i+weekday*workers);
        }
        return today;
    }
    vector<ID> getsens(int weekday){
        vector<ID> today;
        for(auto i:sens){
            today.push_back(i+weekday*workers);
        }
        return today;
    }
    //GUArrantee not empty before calling
    ID fetch(const map<ID,Domain>& D){
        auto mini = D.begin();
        #ifdef INFER
        for(auto p = mini;p!=D.end();p++){
            if((*p).second.size()<(*mini).second.size())
                mini = p;
        }// 最少剩余值
        #endif
        return (*mini).first;
    }
    Domains ConstructStart(){
        map<ID,Domain> temp;
        for(auto a:Base.va){
            Value temp1;
            Value temp2;
            Domain ini;
            int worker = getWorkOrder(a);
            temp1.ok = 0;
            temp1.t = worker_levels[worker];
            temp2.ok = 1;
            temp2.t = worker_levels[worker];
            ini.push_back(temp1);
            ini.push_back(temp2);
            temp[a] = ini;
        }
        return Domains(temp);
    }
    Vars Base;
    bool rel_seted = false;
    bool work_seted = false;
    bool configed = false;
    bool infered = true;
    Domain ini;
    map<ID,vector<VarList>> part_constraint;
    map<int,level> worker_levels;
    int workers = 7;
    int weekdays = 7;
    int atsameday = 4;
    int restlimit = 3;
    int restensure = 2;
    int sen;
    unordered_set<ID> sens;
    //vector<VarList> inner_constraint;
};

}
#include <fstream>
#include <chrono>
void test(ostream& out){
    using namespace First;
    First::Solver Center(7);
    vector<First::level>  levels = {senior,senior,junior,junior,junior,junior,junior};
    Center.set_worker_levels(levels);
    vector<pair<int,int>> hate = {{1,4},{2,3},{3,6}};
    Center.set_part_rel(hate);
    Center.config(4,2);
    Center.test();
    Center.infered = true;
    auto begin  = std::chrono::steady_clock::now();
    string ans = Center.CSP();
    auto end = std::chrono::steady_clock::now();
    cout << "FirstTime:" << double((end-begin).count()/1000)/1000000 << endl;
    cout << "enum:" << Center.count << endl;
    out << ans << endl;
}

void test2(ofstream& out){
    using namespace First;
    First::Solver Center(10);
    vector<First::level>  levels = {senior,senior,junior,junior,
                        junior,junior,junior,senior,junior,senior};
    Center.set_worker_levels(levels);
    vector<pair<int,int>> hate = {{1,5},{2,6},{8,10}};
    Center.set_part_rel(hate);
    Center.config(5,4);
    Center.test();
    Center.infered = true;
    auto begin  = std::chrono::steady_clock::now();
    string ans = Center.CSP();
    auto end = std::chrono::steady_clock::now();
    cout << "SecondTime:" << double((end-begin).count()/1000)/1000000 << endl;
    cout << "enum:" << Center.count << endl;
    out << ans << endl;
}

void testNoInfer(){
    using namespace First;
    First::Solver Center(7);
    vector<First::level>  levels = {senior,senior,junior,junior,junior,junior,junior};
    Center.set_worker_levels(levels);
    vector<pair<int,int>> hate = {{1,4},{2,3},{3,6}};
    Center.set_part_rel(hate);
    Center.config(4,2);
    Center.test();
    Center.infered = false;
    auto begin  = std::chrono::steady_clock::now();
    string ans = Center.CSP();
    auto end = std::chrono::steady_clock::now();
    cout << "NoInferFirstTime:" << double((end-begin).count()/1000)/1000000 << endl;
    cout << "enum:" << Center.count << endl;
}

void testNoInfer2(){
    using namespace First;
    First::Solver Center(10);
    vector<First::level>  levels = {senior,senior,junior,junior,
                        junior,junior,junior,senior,junior,senior};
    Center.set_worker_levels(levels);
    vector<pair<int,int>> hate = {{1,5},{2,6},{8,10}};
    Center.set_part_rel(hate);
    Center.config(5,4);
    Center.test();
    Center.infered = false;
    auto begin  = std::chrono::steady_clock::now();
    string ans = Center.CSP();
    auto end = std::chrono::steady_clock::now();
    cout << "NoInferSecondTime:" << double((end-begin).count()/1000)/1000000 << endl;
    cout << "enum:" << Center.count << endl;
}

int main(void){
    ofstream output1("output1.txt");
    ofstream output2("output2.txt");
    test(output1);
    test2(output2);
    testNoInfer();
    testNoInfer2();
}