import { ABC_REQUESTED } from './constants';
import mainService from './main.service';

export const getAbc = (abc) => async (dispatch) => {
  try {
    const res = await mainService.getAbc({ Text: abc });
    console.log(res);
    dispatch({
      type: ABC_REQUESTED,
      payload: res.data,
    });

    return Promise.resolve(res.data);
  } catch (err) {
    return Promise.reject(err);
  }
};
