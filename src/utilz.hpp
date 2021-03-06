#ifndef UTIL_HPP
#define UTIL_HPP

#include <vector>

using namespace std;


template< typename T >
string to_string(list<T> ll) {
    string res;
    for (T x: ll) {
        res += to_string(x) + " ";
    }
    return res;
}

template< typename T >
string to_string(vector<T> ll) {
    string res;
    for (T x: ll) {
        res += to_string(x) + " ";
    }
    return res;
}



bool is_number(string x) {
    long tt;
    return sscanf(x.c_str(), "%ld", &tt);
}


template< typename T >
T* make() {
    return new T();
}


template< typename T1, typename T2 >
T1* make(T2 x) {
    return new T1(x);
}

template< typename T1, typename T2, typename T3 >
T1* make(T2 x, T3 y) {
    return new T1(x, y);
}



template< typename T >
vector<T> get_tail(const vector<T>& data) {
    vector<T> res;
    auto bb = data.begin();
    bb++;
    res.assign(bb, data.end());
    return res;
}




#endif // UTIL_HPP
