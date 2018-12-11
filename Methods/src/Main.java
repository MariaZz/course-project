import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;
public class Main {
    static int n;
    public static
    void main(String[] args) throws IOException {
        Scanner sc = null, sc1 =null, sc3=null;
        try {
            sc = new Scanner(new FileReader("exchangeRates.in"));
            sc1 = new Scanner(new FileReader("PPT.in"));
            sc3 = new Scanner(new FileReader("economicModel.in"));
            PrintWriter prW = new PrintWriter("result.out");
            n = sc.nextInt();
           double exRates [] = new double[n+1];
           for(int i=0; i<n+1; i++)
               exRates[i]=sc.nextDouble();
           double priceCh1[]=new double[n];
           for (int i=0; i<n; i++)
               priceCh1[i]=sc1.nextDouble();
            double priceCh2[]=new double[n];
            for (int i=0; i<n; i++)
                priceCh2[i]=sc1.nextDouble();
            double first [] =PPT(exRates,priceCh1,priceCh2);
            double second[] =new double[n-1];
            double third[] =new double[n-1];
            for (int i=1;i<n;i++){
                double partOfCurrencyExRates[]= new double[i+1];
                for (int j=0;j<i+1;j++)
                    partOfCurrencyExRates[j]=exRates[j];
                second[i-1]=probabilityTheory(partOfCurrencyExRates, exRates[i+1],i+1);
            }
            double[][]x=new double[n+1][3];
            int i3=0, j3=0;
            while (sc3.hasNext()){
                x[i3][j3]=sc3.nextDouble();
                j3++;
                if(j3==3){
                    i3++;
                    j3=0;
                }
            }
            for (int i=1;i<n;i++){
                double partOfCurrencyExRates[]= new double[i+1];
                double partOfx[][]=new double[i+1][3];
                double newX[]=new double[3];
                for (int j=0;j<i+1;j++) {
                    partOfCurrencyExRates[j] = exRates[j];
                    for(int t=0;t<3;t++)
                        partOfx[j][t]=x[j][t];
                }
                for(int t=0;t<3;t++)
                    newX[t]=x[i+1][t];
                third[i-1]=economicModel(partOfCurrencyExRates,partOfx,newX, i+1);
            }
            for (int i=0;i<n-1;i++)
                first[i]-=exRates[i+1];
            for(int i=0;i<n-1;i++)
                second[i]-=exRates[i+1];
            for(int i=0;i<n-1;i++)
                third[i]-=exRates[i+1];
            double f = result(first), s=result(second),
                    t=result(third);
            if(f>=s && f>=t)
                prW.print("1");
            else {
                if (s>=t)
                    prW.print("2");
                else prW.print("3");
            }
            prW.flush();
        } catch (InputMismatchException e) {
            System.out.println("Incorrect input data!");
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (sc != null) sc.close();
            if (sc1 != null) sc1.close();
            if (sc3 != null) sc3.close();
        }
    }
    static
    double [] PPT(double exRates[], double priceCh1[], double priceCh2[]){
        double result[] =new double[n-1];
        for (int i=1; i<n; i++){
            result[i-1]= (1 + priceCh1[i] - priceCh2[i])*exRates[i];
        }
        return result;
    }

    static
    double probabilityTheory (double pair_ini[], double current_piar, int n2){
        double ten=1;
        if(pair_ini[0]>=10){
            while (pair_ini[0]>=10){
                ten*=10;
                pair_ini[0]/=10;
            }
            for (int i=1;i<n2;i++)
                pair_ini[i]/=ten;
            current_piar/=ten;
        }else {
            if(pair_ini[0]<1){
                while (pair_ini[0]<1){
                    ten/=10;
                    pair_ini[0]*=10;
                }
                for (int i=1;i<n2;i++)
                    pair_ini[i]/=ten;
                current_piar/=ten;
            }
        }
        int coeff = 100;
        double m1 = pair_ini[0];
        double m2= pair_ini[0];
        int up=0;
        for (int i=1;i<n2;i++){
            m2=Math.min(m2,pair_ini[i]);
            m1=Math.max(m1,pair_ini[i]);
            if(pair_ini[i-1]<pair_ini[i])
               up++;
        }
        m1=Math.max(m1,current_piar);
        m2=Math.min(m2,current_piar);
        m1*=coeff;
        m2*=coeff;
        int M=(int)(m1-m2);
        M++;
        int min=(int)m2;
        double P[][]=new double[M][M];
        double p;
        if(up!=0 && up!=n2)
            p = up/n2;
        else {
            if(up==0) p=1/n2;
            else p=(n2-1)/n2;
        }
        P[0][0]=1;
        P[M-1][M-1]=1;
        for (int i=1;i<M-1;i++){
            P[i][i-1]=1-p;
            P[i][i+1]=p;
        }
        int num=(int)(current_piar*coeff-min);
        if(num==M) num--;
        double P2[][]=new double[M][M];
        for (int i=0;i<M;i++){
            for (int j=0;j<M;j++) {
                for (int t = 0; t < M; t++)
                    P2[i][j]+=P[i][t]*P[j][t];
            }
        }
        double s[]=new double[M];
        for (int i=0;i<M;i++)
            s[i]=P2[num][i];
        int k=0, ind_max=0;
        double max2=0;
        for (int i=0; i< M; i++){
            if(s[i]> max2){
                ind_max= k;
                max2 = s[i]; }
            k++;
        }
        int sum_l=0;
        int sum_r=0;
        for(k=0;k<ind_max;k++){
            sum_l +=s[k];
        }
        for(k=ind_max+1;k<M;k++){
            sum_r +=s[k];
        }
        int excess = sum_l - sum_r;
        double x = M*excess/2;
        return  (1 + (ind_max - x + min)/coeff)*ten;
    }

    static
    double economicModel(double[] y, double[][]x, double [] newX, int n3){
        int N=4;
        double B[]=new double[N];
        double A[][]=new double[N][N];
        for(int i=0;i<N;i++){
            for(int j=0;j<n3;j++){
                if(i==0) B[i]+=y[j];
                else B[i]+=y[j]*x[j][i-1];
            }
        }
        for (int i=0;i<N;i++){
            for (int j=0;j<N; j++){
                if(i==0 || j==0){
                    if(i==0 && j==0) A[i][j]=n3;
                    else {
                       for (int t=0;t<n3;t++) {
                           if (i == 0) A[i][j] += x[t][j-1];
                           else A[i][j]+=x[t][i-1];
                       }
                    }
                }
                else {
                    for (int t=0;t<n3;t++)
                        A[i][j]+=x[t][j-1]*x[t][i-1];
                }
            }
        }
        double X[]=new double[N];
        for (int i=0;i< N-1;i++){
             for (int j= i+1; j< N; j++){
                double a=-1*A[j][i]/A[i][i];
                A[j][i]=0;
                for(int t=i+1;t<N;t++){
                    A[j][t]+=a*A[i][t];
                }
                    B[j]+=B[i]*a;
             }
        }
        X[N-1]=B[N-1]/A[N-1][N-1];
        for(int i=N-2; i>=0; i--){
            double s=0;
            for (int j=i+1; j<N; j++){
                s+=X[j]*A[i][j];
            }
            s=B[i]-s;
            X[i]=s/A[i][i];
        }
        double newY=X[0];
        for (int i=1; i<N;i++){
            newY+=X[i]*newX[i-1];
        }
        return newY;
    }
    static
    double result(double method []) {
        double index = 0;
        double t=n*n;
        double coeff = 1/t;
        double last=0;
        boolean down[] = new boolean[n-1];
        for(int i=0;i<n-2;i++){
            index-=coeff*Math.abs(method[i]-method[i+1]);
            if(i>0)
                index+=coeff*(last - Math.abs(method[i]-method[i+1]));
            coeff = (i+1)/(n*n);
            last = Math.abs(method[i]-method[i+1]);
        }

        for (int i=0;i<n-1;i++)
            method[i]=Math.abs(method[i]);
        last=method[0];
        coeff=1/t;

        for (int i=0;i<n-1;i++){
            index-=coeff*method[i];
            if (last>=method[i])
                down[i]=true;
            if(i>0)
                index+=coeff*(last - method[i]);
            coeff = (i+1)/(n*n);
            last = method[i];
        }
        coeff=1/t;
        last=0;
        boolean lastB=down[0];
        for(int i=1; i<n-1;i++){
            if(lastB!=down[i]){
                index+=coeff*(last/n);
                last=0;
            }
            else last++;
            lastB=down[i];
        }
        if(last==n-1) index+=coeff*(last/n);
        return index;
    }
}