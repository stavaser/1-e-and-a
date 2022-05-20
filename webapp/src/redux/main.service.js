import http from './ApiClient';

class MainService {
  getAbc(abc) {
    return http.post(`/api/1eaToAbc/`, abc);
  }
}

export default new MainService();
