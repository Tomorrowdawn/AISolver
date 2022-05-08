// C++ 定义如下：
#include<vector>
#include <string>
#include <unordered_map>
#include <iostream>
#include <queue>
#include <algorithm>
#include <unordered_set>
#include <cstring>
using namespace std;
using matrix = vector<vector<int>>;
#include <map>

class State{
   public:
    State()=default;
    State(int nil){NIL = nil;}
    State(const matrix& M):Distribution{M}{
        zip = "";
        this->n = M.size();
        for(int i=0;i<n;i++){
            for(int j=0;j<n;j++){
                if(M[i][j]>0)
                    zip += (char)M[i][j];
                else if(M[i][j]==0){
                    position.first = i;
                    position.second = j;
                    zip+= (char)127;
                }
                else{
                    zip += (char)n;
                }
            }
        }
    }
    bool isBlackHole(pair<int,int> p){
        return Distribution[p.first][p.second] < 0;
    }
    std::string print(){
        string z = "";
        for(int i=0;i<n;i++){
            for(int j=0;j<n;j++){
                z += to_string(Distribution[i][j]) + ',';
            }
            z += "\n";
        }
        return z;
    }
    int h(const State& target);
    inline int f() const{
        return history + heu;
    }
    State Action(char direction){
        State Current(*this);
        int x = position.first;
        int y = position.second;
        if(direction=='D'){
            return Current.swap(position,{x+1,y}).set_op('D',history);
        }
        else if(direction=='U'){
            return Current.swap(position,{x-1,y}).set_op('U',history);
        }
        else if(direction=='L'){
            return Current.swap(position,{x,y-1}).set_op('L',history);
        }
        else if(direction=='R'){
            return Current.swap(position,{x,y+1}).set_op('R',history);
        }
        else
            throw "Error Command";
    }
    State swap(pair<int,int> i,pair<int,int> j){
        //cerr << "i,j=" << i.first << i.second <<"," << j.first << j.second << endl;
        if(i.first==2&&i.first==j.first){
            int y1 = (i.second<0)?4:i.second%5;
            int y2 = (j.second<0)?4:j.second%5;
            matrix ac = Distribution;
            ac[2][y1] = Distribution[2][y2];
            ac[2][y2] = Distribution[2][y1];
            return State(ac);
        }
        if(i.second==2&&i.second==j.second){
            int x1 = (i.first==-1)?4:i.first%5;
            int x2 = (j.first==-1)?4:j.first%5;
            matrix ac = Distribution;
            ac[x1][2] = Distribution[x2][2];
            ac[x2][2] = Distribution[x1][2];
            return State(ac);
        }
        if(i.first>=n||i.second>=n||j.second>=n||j.first>=n)
            return State(-1);
        if(i.first<0||i.second<0||j.second<0||j.first<0)
            return State(-1);
        if(isBlackHole(i)||isBlackHole(j))
            return State(-1);
        matrix ac = Distribution;
        ac[i.first][i.second] = Distribution[j.first][j.second];
        ac[j.first][j.second] = Distribution[i.first][i.second];
        return State(ac);
    }
    inline State set_op(char p,int his){op = p;this->history = his + 1; return *this;}
    matrix Distribution;
    int NIL = 0;
    std::string zip;
    char op;
    int heu = -1;
    int history = 0;
   protected:
    pair<int,int> position = {-1,-1};
    int n;
};

bool operator == (const State& lhs,const State& rhs){
        return lhs.zip == rhs.zip;
}

bool operator<(const State& lhs,const State& rhs){
        return lhs.f() > rhs.f();
    }

int State::h(const State& target){
    // simple apply
    int result = 0;
    for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
            if(Distribution[i][j]>=0&&Distribution[i][j]!=target.Distribution[i][j]){
                result ++;
            }
        }
    }
    heu = result;
    return result;
}

namespace std{
template<>
struct hash<State>{
    std::size_t operator()(const State & K)const{
        return hash<string>()(K.zip);
    }
};
}

#include <list>

class SuperSlow{
  public:
    State top(){
        if(Queue.empty()) return State(-1);
        auto mini = Queue.begin();
        for(auto p = Queue.begin();p!=Queue.end();p++){
            if(*p<*mini){
                mini = p;
            }
        }
        State res = *mini;
        Queue.erase(mini);
        return res;
    }
    inline void push(State S){
        Queue.push_back(S);
    }
    inline bool empty(){
        return Queue.empty();
    }
    string print(){
        string result = "";
        for(auto p:Queue){
            result += p.print() + "\n";
        }
        return result;
    }
    list<State> Queue;
};

class Solver{
   public:
    Solver(){NIL.NIL = -1;}
    Solver(const matrix& start,const matrix& goal){
        Current = State(start);
        Goal = State(goal);
        NIL.NIL = -1;
    }
    vector<State> Actions(State K){
        vector<State> result;
        
        vector<char> P = {'R','L','U','D'};
        for(auto op:P){
            auto R = K.Action(op);//
            if(R.NIL!=NIL.NIL){
                result.push_back(R);
            }
        }
        //cerr << "resultsize=" << result.size() << endl;
        return result;
    }
    std::string Astar(){
        unordered_map<State,std::pair<State,char> > Parents;
        //child,<parent,op> (parent do op and transform to child)
        priority_queue<State> HQ;
        //SuperSlow HQ;
        unordered_set<string> visited;
        Current.h(Goal);
        HQ.push(Current);
        Parents[Current] = {NIL,1};
        int count = 0;
        while(!HQ.empty()){
            auto S = HQ.top();
            HQ.pop();
            visited.emplace(S.zip);
            for(auto P:Actions(S)){
                if(visited.find(P.zip)==visited.end()){
                    P.h(Goal);
                    Parents[P] = {S,P.op};
                    if(P==Goal){
                        string result = "";
                        while(Parents[P].second!=1){
                            result = Parents[P].second + result;
                            P = Parents[P].first;
                        }
                        return result;
                    }
                    HQ.push(P);
                }
            }
        }
        return "";
    }
    const int infty = 10000000;
    class Path{
       public:
        Path(){
            last.NIL = -1;
            last.op = 1;
        }
        auto find(State& n){
            return P.find(n);
        }
        auto end(){
            return P.end();
        }
        inline State back(){
            return last;
        }
        inline void push(State n){
            P[n] = {last,n.op};
            last = n;
        }
        inline void pop(){
            State cp = P[last].first;
            P.erase(last);
            last = cp;
        }
        unordered_map<State,std::pair<State,char> > P;
        State last;
    };
    virtual std::string IDAstar(void){
        Path Parents;
        int bound = Current.h(Goal);
        Current.op = 1;
        Parents.push(Current);
        int t = infty;
        do{
            t = IDASearch(Parents,bound);
            if(t==-1){
                string result = "";
                while(Parents.back().op!=1){
                    result = Parents.back().op + result;
                    Parents.pop();
                }
                return result;
            }
            bound = t;
        }while(t!=infty);
        return "";
    }
    virtual int IDASearch(Path& parents,int bound){
        auto Node = parents.back();
        int f = Node.f();
        if(f>bound)
            return f;
        if(Node==Goal){
            return -1;
        }
        int min = infty;
        for(auto P:Actions(Node)){
            if(parents.find(P)==parents.end()){
                P.h(Goal);
                parents.push(P);
                int t = IDASearch(parents,bound);
                if(t==-1)
                    return -1;//外面直接find Goal即可.
                if(t<min)
                    min = t;
                parents.pop();
            }
        }
        return min;
    }
    int count = 0;
   protected:
    int n;
    State Current;
    State Goal;
    State NIL;
};

struct TargetLockOn{
    pair<int,int>& operator[](int id){
        return targetrecord[id];
    }
    map<int,pair<int,int>> targetrecord;
};

class Solver2:public Solver{
   public:
    Solver2(const matrix& start,const matrix& goal){
        Current = State(start);
        Goal = State(goal);
        CalCulateTarget();
        NIL.NIL = -1;
    }
    void CalCulateTarget(){
        int n = Goal.Distribution.size();
        for(int i=0;i<n;i++){
            for(int j=0;j<n;j++){
                TA[Goal.Distribution[i][j]] = {i,j};
            }
        }
    }
     virtual std::string IDAstar(void) final{
        Path Parents;
        int bound = improvedH(Current);
        Current.heu = bound;
        Current.op = 1;
        Parents.push(Current);
        int t = infty;
        do{
            t = IDASearch(Parents,bound);
            if(t==-1){
                string result = "";
                while(Parents.back().op!=1){
                    result = Parents.back().op + result;
                    Parents.pop();
                }
                return result;
            }
            bound = t;
        }while(t!=infty);
        return "";
    }
    virtual int IDASearch(Path& parents,int bound) final{
        auto Node = parents.back();
        int f = Node.f();
        if(f>bound)
            return f;
        if(Node==Goal){
            return -1;
        }
        int min = infty;
        for(auto P:Actions(Node)){
            if(parents.find(P)==parents.end()){
                P.heu = improvedH(P);
                parents.push(P);
                int t = IDASearch(parents,bound);
                if(t==-1)
                    return -1;//外面直接find Goal即可.
                if(t<min)
                    min = t;
                parents.pop();
            }
        }
        return min;
    }
    int improvedH(State& cur){
        // simple apply
        int result = 0;
        int n = 5;
        for(int i=0;i<n;i++){
            for(int j=0;j<n;j++){
                int star = cur.Distribution[i][j];
                auto t = TA[star];
                int x = t.first;
                int y = t.second;
                //不走隧道
                int norm = abs(x-i)+abs(y-j);
                //走横向隧道
                int horizon = abs(j-2) + abs(y-2);//纵向距离
                if(x>i)
                    horizon+= i + 5 - x;
                else
                    horizon += x + 5 - i;
                //走纵向隧道.由于两条隧道都在中间,最多走一次隧道,不然肯定亏
                int vertical = abs(x-2)+abs(i-2);//横向距离
                if(y>j)
                    vertical = j+5-y;
                else
                    vertical = y+5-j;
                result += min({norm,horizon,vertical});
            }
        }
        return result;
    }
    std::string betterAstar(){
        unordered_map<State,std::pair<State,char> > Parents;
        //child,<parent,op> (parent do op and transform to child)
        priority_queue<State> HQ;
        //SuperSlow HQ;
        unordered_set<string> visited;
        Current.h(Goal);
        HQ.push(Current);
        Parents[Current] = {NIL,1};
        while(!HQ.empty()){
            auto S = HQ.top();
            HQ.pop();
            visited.emplace(S.zip);
            for(auto P:Actions(S)){
                if(visited.find(P.zip)==visited.end()){
                    P.heu = improvedH(P);
                    Parents[P] = {S,P.op};
                    if(P==Goal){
                        string result = "";
                        while(Parents[P].second!=1){
                            result = Parents[P].second + result;
                            P = Parents[P].first;
                        }
                        return result;
                    }
                    HQ.push(P);
                }
            }
        }
        return "";
    }
   protected:
    TargetLockOn TA;
};

#include <fstream>
#include <chrono>


void A_h1(const vector<vector<int> > &start, const vector<vector<int> >
&target){
    auto begin  = std::chrono::steady_clock::now();
    Solver S(start,target);
    string an = S.Astar();
    auto end = std::chrono::steady_clock::now();
    cout << an << "," << double((end-begin).count()/1000)/1000000 << endl;
}

void A_h2(const vector<vector<int> > &start, const vector<vector<int> >
&target){
    auto begin  = std::chrono::steady_clock::now();
    Solver2 S(start,target);
    string an = S.betterAstar();
    auto end = std::chrono::steady_clock::now();
    cout << an << "," << double((end-begin).count()/1000)/1000000 << endl;
}


void IDA_h1(const vector<vector<int> > &start, const vector<vector<int> >
&target){
    auto begin  = std::chrono::steady_clock::now();
    Solver S(start,target);
    string an = S.IDAstar();
    auto end = std::chrono::steady_clock::now();
    cout << an << "," << double((end-begin).count()/1000)/1000000 << endl;
}

void IDA_h2(const vector<vector<int> > &start, const vector<vector<int> >
&target){
    auto begin  = std::chrono::steady_clock::now();
    Solver2 S(start,target);
    string an = S.IDAstar();
    auto end = std::chrono::steady_clock::now();
    cout << an << "," << double((end-begin).count()/1000)/1000000 << endl;
}

int main(int argc,const char** argv){
    vector<string> S = {};
    for(int i=0;i<=11;i++){
        if(i>=10)
            S.push_back(to_string(i));
        else
            S.push_back("0"+to_string(i));
    }
    if(strcmp(argv[1],"A_h1")==0){
        ofstream ans1("output_A_h1.txt");
        cout.rdbuf(ans1.rdbuf());
        for(int k=0;k<12;k++){
        matrix Start(5,vector<int>(5,0)),goal(5,vector<int>(5,0));
        ifstream Input("./data/input"+S[k]+".txt");
        ifstream T("./data/target"+S[k]+".txt");
        for(int i=0;i<5;i++){
            for(int j=0;j<5;j++){
                Input >> Start[i][j];
                T >> goal[i][j];
            }
        }
        A_h1(Start,goal);
    }
    }
    else if(strcmp(argv[1],"A_h2")==0){
        ofstream ans2("output_A_h2.txt");
        cout.rdbuf(ans2.rdbuf());
        for(int k=0;k<12;k++){
        matrix Start(5,vector<int>(5,0)),goal(5,vector<int>(5,0));
        ifstream Input("./data/input"+S[k]+".txt");
        ifstream T("./data/target"+S[k]+".txt");
        for(int i=0;i<5;i++){
            for(int j=0;j<5;j++){
                Input >> Start[i][j];
                T >> goal[i][j];
            }
        }
        A_h2(Start,goal);
    }
    }
    else if(strcmp(argv[1],"IDA_h1")==0){
        ofstream ans3("output_IDA_h1.txt");
        cout.rdbuf(ans3.rdbuf());
        for(int k=0;k<12;k++){
        matrix Start(5,vector<int>(5,0)),goal(5,vector<int>(5,0));
        ifstream Input("./data/input"+S[k]+".txt");
        ifstream T("./data/target"+S[k]+".txt");
        for(int i=0;i<5;i++){
            for(int j=0;j<5;j++){
                Input >> Start[i][j];
                T >> goal[i][j];
            }
        }
        IDA_h1(Start,goal);
        }
    }
    else if(strcmp(argv[1],"IDA_h2")==0){
        ofstream ans4("output_IDA_h2.txt");
        cout.rdbuf(ans4.rdbuf());
        for(int k=0;k<12;k++){
        matrix Start(5,vector<int>(5,0)),goal(5,vector<int>(5,0));
        ifstream Input("./data/input"+S[k]+".txt");
        ifstream T("./data/target"+S[k]+".txt");
        for(int i=0;i<5;i++){
            for(int j=0;j<5;j++){
                Input >> Start[i][j];
                T >> goal[i][j];
            }
        }
        IDA_h2(Start,goal);
        }
    }
    else
        return 1;
    return 0;
}