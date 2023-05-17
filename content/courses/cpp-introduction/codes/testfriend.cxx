class ClassA;

class ClassB {
   int i,j;
   public:
      void set_ij(int,int);
      int get_i(int);
      float multipy_ij(ClassB b);
};

class ClassA {
   friend class ClassB;
   float x,y;
   public:
      void set_xy(float,float,int,int);  //use values from ClassB
      float get_xy(float,float);
};

