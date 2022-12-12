#include <iostream>
#include <vector>
#include <queue>
#include <string>
#include <limits>

using namespace std;
using ll = long long;
using tt = pair<ll, pair<ll, ll>>;
const char nl = '\n';
const ll inf = numeric_limits<ll>::max();

vector<string> a;
ll w, h = 0;
ll sx, sy, ex, ey;
vector<vector<ll>> d;
vector<vector<bool>> v;
priority_queue<tt, vector<tt>, greater<tt>> q;

vector<pair<ll, ll>> neighbors(ll y, ll x) {
    vector<pair<ll, ll>> ret;
    char t = a[y][x] + 1;
    // up
    if (y > 0 && a[y - 1][x] <= t) {
        ret.push_back({y - 1, x});
    }
    // down
    if (y + 1 < h && a[y + 1][x] <= t) {
        ret.push_back({y + 1, x});
    }
    // left
    if (x > 0 && a[y][x - 1] <= t) {
        ret.push_back({y, x - 1});
    }
    // right
    if (x + 1 < w && a[y][x + 1] <= t) {
        ret.push_back({y, x + 1});
    }
    return ret;
}

vector<pair<ll, ll>> all_as() {
    //cout << "finding as\n";
    vector<pair<ll, ll>> ret;
    for (ll y = 0; y < h; y++) {
        for (ll x = 0; x < w; x++) {
            //cout << a[y][x] << nl;
            if (a[y][x] == 'a') {
                ret.push_back({y, x});
            }
        }
    }
    return ret;
}

int main() {
    string line;
    while (getline(cin, line)) {
        size_t k;
        if ((k = line.find('S')) != string::npos) {
            sx = k;
            sy = h;
        }
        if ((k = line.find('E')) != string::npos) {
            ex = k;
            ey = h;
        }
        a.push_back(line);
        h++;
    }
    a[sy][sx] = 'a';
    a[ey][ex] = 'z';
    w = a[0].length();
    ll lowest = inf;
    for (auto [ay, ax] : all_as()) {
        //cout << ay << "," << ax << nl;
        sy = ay; sx = ax;
        d = vector(h, vector(w, inf));
        v = vector(h, vector(w, false));
        d[sy][sx] = 0;
        q.push({0, {sy, sx}});
        while (!q.empty()) {
            auto [kd, koords] = q.top();
            auto [ky, kx] = koords;
            q.pop();
            if (v[ky][kx]) {
                continue;
            }
            //cout << "processing " << ky << "," << kx << " with dist " << kd << nl;
            v[ky][kx] = true;
            for (auto [ny, nx] : neighbors(ky, kx)) {
                //cout << "\tneighbor " << ny << "," << nx;
                // cost always 1
                ll new_d = d[ky][kx] + 1;
                if (new_d < d[ny][nx]) {
                    d[ny][nx] = new_d;
                    //cout << ": new_d " << new_d;
                    q.push({new_d, {ny, nx}});
                }
                //cout << nl;
            }
        }
        lowest = min(lowest, d[ey][ex]);
    }
    cout << lowest << nl;
    return 0;
}

