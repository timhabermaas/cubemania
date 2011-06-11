#include <mysql.h>
#include <stdio.h>

#include <iostream>
#include <vector>
#include <cfloat>
#include <algorithm>
#include <cstring>

struct Single {
  long id;
  long time;
  bool dnf;

  Single(long id, long time, bool dnf) : id(id), time(time), dnf(dnf) {
  }

  bool operator<(const Single& other) const {
    if (dnf)
      return false;
    if (other.dnf)
      return true;
    return time < other.time;
  }
};

struct RollingAverage {
  int size;
  int dnfs;
  std::vector<Single> singles;

  RollingAverage(int size) : size(size), dnfs(0) {
  }

  void add(Single single) {
    singles.push_back(single);
    if (single.dnf) {
      dnfs++;
    }

    if (singles.size() > size) {
      if (singles[0].dnf) {
        dnfs--;
      }
      singles.erase(singles.begin());
    }
  }

  float average() {
    std::vector<Single> temp = singles;
    if (dnfs > 1 || singles.size() < size) {
      return -1;
    }
    std::sort(temp.begin(), temp.end());
    temp.erase(temp.begin());
    temp.erase(temp.end());
    long sum = 0;
    for (int i = 0; i < temp.size(); i++) {
      sum += temp[i].time;
    }
    return sum / (float)temp.size();
  }

  std::vector<Single> getSingles() {
    return singles;
  }
};

int main(int argc, char *argv[]) {
  MYSQL *conn;
  MYSQL_RES *result;
  MYSQL_ROW row;

  if (argc != 4) {
    std::cout << "Usage: average user_id puzzle_id {5, 12}" << std::endl;
    return 1;
  }

  conn = mysql_init(NULL);

  if (mysql_real_connect(conn, "localhost", getenv("CUBEMANIA_USERNAME"), getenv("CUBEMANIA_PASSWORD"), getenv("CUBEMANIA_DATABASE"), 0, NULL, 0) == NULL) {
    std::cout << "ERROR: Can't connect to database" << std::endl;
    return 1;
  }


  int user_id = atoi(argv[1]);
  int puzzle_id = atoi(argv[2]);
  int amount = atoi(argv[3]);

  char buffer[1024];
  sprintf(buffer, "SELECT id, time, penalty from singles WHERE user_id = %d AND puzzle_id = %d ORDER BY created_at desc;", user_id, puzzle_id);
  mysql_query(conn, buffer);
  result = mysql_store_result(conn);

  RollingAverage ra(amount);
  float best = FLT_MAX;
  std::vector<Single> bestSingles;

  while (row = mysql_fetch_row(result)) {
    ra.add(Single(atoi(row[0]), atoi(row[1]), row[2] ? strcmp(row[2], "dnf") : false));
    if (ra.average() < best && ra.average() > 0) {
      best = ra.average();
      bestSingles = ra.getSingles();
    }
  }

  if (best == FLT_MAX)
    std::cout << "NULL";
  else {
    std::cout << best;
    for (int i = 0; i < bestSingles.size(); i++) {
      std::cout << ", " << bestSingles[i].id;
    }
  }


  mysql_free_result(result);
  mysql_close(conn);
  return 0;
}